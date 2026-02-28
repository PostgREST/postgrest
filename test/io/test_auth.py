"Auth related IO tests for PostgREST"

from datetime import datetime, timedelta, timezone
from operator import attrgetter
import signal
import time
import pytest

from config import BASEDIR, CONFIGSDIR, FIXTURES, SECRET
from util import authheader, jwtauthheader, parse_server_timings_header
from postgrest import (
    run,
    sleep_until_postgrest_config_reload,
    sleep_until_postgrest_scache_reload,
    wait_until_exit,
)


@pytest.mark.parametrize(
    "secretpath",
    [path for path in (BASEDIR / "secrets").iterdir() if path.suffix != ".jwt"],
    ids=attrgetter("name"),
)
def test_read_secret_from_file(secretpath, defaultenv):
    "Authorization should succeed when the secret is read from a file."

    env = {**defaultenv, "PGRST_JWT_SECRET": f"@{secretpath}"}

    if secretpath.suffix == ".b64":
        env["PGRST_JWT_SECRET_IS_BASE64"] = "true"

    secret = secretpath.read_bytes()
    headers = authheader(secretpath.with_suffix(".jwt").read_text())

    with run(stdin=secret, env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        print(response.text)
        assert response.status_code == 200


def test_read_secret_from_stdin(defaultenv):
    "Authorization should succeed when the secret is read from stdin."

    env = {**defaultenv, "PGRST_DB_CONFIG": "false", "PGRST_JWT_SECRET": "@/dev/stdin"}

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(stdin=SECRET.encode(), env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        print(response.text)
        assert response.status_code == 200


# TODO: This test would fail right now, because of
# https://github.com/PostgREST/postgrest/issues/2126
@pytest.mark.skip
def test_read_secret_from_stdin_dbconfig(defaultenv):
    "Authorization should succeed when the secret is read from stdin with db-config=true."

    env = {**defaultenv, "PGRST_DB_CONFIG": "true", "PGRST_JWT_SECRET": "@/dev/stdin"}

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(stdin=SECRET.encode(), env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        print(response.text)
        assert response.status_code == 200


def test_jwt_errors(defaultenv):
    "invalid JWT should throw error"

    env = {**defaultenv, "PGRST_JWT_SECRET": SECRET, "PGRST_JWT_AUD": "io tests"}

    def relativeSeconds(sec):
        return int((datetime.now(timezone.utc) + timedelta(seconds=sec)).timestamp())

    with run(env=env) as postgrest:
        headers = jwtauthheader({}, "other secret")
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "No suitable key or wrong key type"
        assert (
            response.json()["details"] == "None of the keys was able to decode the JWT"
        )

        headers = jwtauthheader({"role": "not_existing"}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == 'role "not_existing" does not exist'

        # -35 seconds, because we allow clock skew of 30 seconds
        headers = jwtauthheader({"exp": relativeSeconds(-35)}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWT expired"

        # 35 seconds, because we allow clock skew of 30 seconds
        headers = jwtauthheader({"nbf": relativeSeconds(35)}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWT not yet valid"

        # 35 seconds, because we allow clock skew of 35 seconds
        headers = jwtauthheader({"iat": relativeSeconds(35)}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWT issued at future"

        headers = jwtauthheader({"aud": "not set"}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWT not in audience"

        # partial token, no signature
        headers = authheader("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.bm90IGFuIG9iamVjdA")
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "Expected 3 parts in JWT; got 2"

        # complete token but random characters
        headers = authheader("quifquirndsjagnrgniur.fonvoienqhhdj.iuqvnvhojah")
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWT cryptographic operation failed"

        # token with algorithm "none"
        headers = authheader(
            "eyJ0eXAiOiJKV1QiLCJhbGciOiJub25lIn0.e30.yOBhlOIqn56T-4NvyEXCjfi3UmyQZ-BzXtePMO2NgRI"
        )
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "Wrong or unsupported encoding algorithm"
        assert (
            response.json()["details"]
            == "JWT is unsecured but expected 'alg' was not 'none'"
        )

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_ENTRIES": "86400",
        "PGRST_JWT_SECRET": SECRET,
    }

    # for code coverage with cache enabled and server-timing enabled
    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only")
        assert response.status_code == 401
        assert response.json()["message"] == "permission denied for table authors_only"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "false",
        "PGRST_JWT_CACHE_MAX_ENTRIES": "86400",
        "PGRST_JWT_SECRET": SECRET,
    }

    # for code coverage with cache enabled and server-timing disabled
    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only")
        assert response.status_code == 401
        assert response.json()["message"] == "permission denied for table authors_only"


def test_fail_with_invalid_password(defaultenv):
    "Connecting with an invalid password should fail without retries."
    uri = f'postgresql://?dbname={defaultenv["PGDATABASE"]}&host={defaultenv["PGHOST"]}&port={defaultenv["PGPORT"]}&user=some_protected_user&password=invalid_pass'
    env = {**defaultenv, "PGRST_DB_URI": uri}
    with run(env=env, wait_for_readiness=False) as postgrest:
        exitCode = wait_until_exit(postgrest)
        assert exitCode == 1


@pytest.mark.parametrize(
    "roleclaim", FIXTURES["roleclaims"], ids=lambda claim: claim["key"]
)
def test_role_claim_key(roleclaim, defaultenv):
    "Authorization should depend on a correct role-claim-key and JWT claim."
    env = {
        **defaultenv,
        "PGRST_JWT_ROLE_CLAIM_KEY": roleclaim["key"],
        "PGRST_JWT_SECRET": SECRET,
    }
    headers = jwtauthheader(roleclaim["data"], SECRET)

    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == roleclaim["expected_status"]


@pytest.mark.parametrize(
    "jwtaudroleclaim",
    FIXTURES["jwtaudroleclaims"],
    ids=lambda claim: claim["key"] + "_" + str(claim["expected_status"]),
)
def test_jwt_aud_in_role_claim_key(jwtaudroleclaim, defaultenv):
    "Allows authorization with JWT aud claim in role-claim-key"

    env = {
        **defaultenv,
        "PGRST_JWT_AUD": "postgrest_test_author",
        "PGRST_JWT_ROLE_CLAIM_KEY": jwtaudroleclaim["key"],
        "PGRST_JWT_SECRET": SECRET,
    }

    headers = jwtauthheader(jwtaudroleclaim["data"], SECRET)

    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == jwtaudroleclaim["expected_status"]


def test_iat_claim(defaultenv):
    """
    A claim with an 'iat' (issued at) attribute should be successful.

    The PostgREST time cache leads to issues here, see:
    https://github.com/PostgREST/postgrest/issues/1139

    """

    env = {**defaultenv, "PGRST_JWT_SECRET": SECRET}

    claim = {"role": "postgrest_test_author", "iat": datetime.now(timezone.utc)}
    headers = jwtauthheader(claim, SECRET)

    with run(env=env) as postgrest:
        for _ in range(10):
            response = postgrest.session.get("/authors_only", headers=headers)
            assert response.status_code == 200

            time.sleep(0.1)


def test_jwt_secret_reload(tmp_path, defaultenv):
    "JWT secret should be reloaded from file when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(configfile, env=defaultenv) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # change setting
        configfile.write_text(config.replace("invalid" * 5, SECRET))

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)

        sleep_until_postgrest_config_reload()

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200


def test_jwt_secret_external_file_reload(tmp_path, defaultenv):
    "JWT secret external file should be reloaded when PostgREST is sent a SIGUSR2 or a NOTIFY."
    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    external_secret_file = tmp_path / "jwt-secret-config"
    external_secret_file.write_text("invalid" * 5)

    env = {
        **defaultenv,
        "PGRST_JWT_SECRET": f"@{external_secret_file}",
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGRST_DB_CONFIG": "false",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",  # required for NOTIFY
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # change external file
        external_secret_file.write_text(SECRET)

        # SIGUSR1 doesn't reload external files, at least when db-config=false
        postgrest.process.send_signal(signal.SIGUSR1)
        sleep_until_postgrest_scache_reload()

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # reload config and external file with SIGUSR2
        postgrest.process.send_signal(signal.SIGUSR2)
        sleep_until_postgrest_config_reload()

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200

        # change external file to wrong value again
        external_secret_file.write_text("invalid" * 5)

        # reload config and external file with NOTIFY
        response = postgrest.session.post("/rpc/reload_pgrst_config")
        assert response.text == ""
        assert response.status_code == 204
        sleep_until_postgrest_config_reload()

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401


# TODO: This test is more related to observability than authentication.
#       So, move it an appropriate test module.
def test_jwt_cache_server_timing(defaultenv):
    "server-timing duration is exposed for JWT with expiry"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_ENTRIES": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    headers = jwtauthheader(
        {
            "role": "postgrest_test_author",
            "exp": int(
                (datetime.now(timezone.utc) + timedelta(minutes=30)).timestamp()
            ),
        },
        SECRET,
    )

    with run(env=env) as postgrest:
        first = postgrest.session.get("/authors_only", headers=headers)
        second = postgrest.session.get("/authors_only", headers=headers)

        assert first.status_code == 200
        assert second.status_code == 200

        first_dur = parse_server_timings_header(first.headers["Server-Timing"])["jwt"]
        second_dur = parse_server_timings_header(second.headers["Server-Timing"])["jwt"]

        # with jwt caching the parse time of second request with the same token
        # should be at least as fast as the first one
        assert second_dur <= first_dur


def test_jwt_cache_without_server_timing(defaultenv):
    "JWT cache does not break requests with server-timing disabled"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "false",
        "PGRST_JWT_CACHE_MAX_ENTRIES": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(env=env) as postgrest:
        first = postgrest.session.get("/authors_only", headers=headers)
        second = postgrest.session.get("/authors_only", headers=headers)

        assert first.status_code == 200
        assert second.status_code == 200


def test_jwt_cache_without_exp_claim(defaultenv):
    "server-timing duration is exposed for JWT without expiry"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_ENTRIES": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)  # no exp

    with run(env=env) as postgrest:
        first = postgrest.session.get("/authors_only", headers=headers)
        second = postgrest.session.get("/authors_only", headers=headers)

        assert first.status_code == 200
        assert second.status_code == 200

        first_dur = parse_server_timings_header(first.headers["Server-Timing"])["jwt"]
        second_dur = parse_server_timings_header(second.headers["Server-Timing"])["jwt"]

        assert first_dur >= 0
        assert second_dur >= 0


def test_invalidate_jwt_cache_when_secret_changes(tmp_path, defaultenv):
    "JWT cache should be emptied after jwt-secret is changed in a config reload"

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    external_secret_file = tmp_path / "jwt-secret-config"
    external_secret_file.write_text(SECRET)

    env = {
        **defaultenv,
        "PGRST_JWT_SECRET": f"@{external_secret_file}",
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_ENTRIES": "86400",  # enable cache
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",  # required for NOTIFY
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200  # jwt gets cached

        # change external file
        external_secret_file.write_text("invalid" * 5)

        # reload config and external file with NOTIFY
        # jwt-cache should get empty
        response = postgrest.session.post("/rpc/reload_pgrst_config")
        assert response.text == ""
        assert response.status_code == 204
        sleep_until_postgrest_config_reload()

        # now the request should fail because the cached token is removed
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401
