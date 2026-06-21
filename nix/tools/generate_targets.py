# generates a file to be used by the vegeta load testing tool

# It includes a worst case scenario for the JWT cache:
# - all requests will have a unique JWT so no cache hits
# - all jwts have an expiration that will be long enough to be
#   valid at time of request but short enough that already
#   validated jwts will expire later during the loadtest run
# - the above guarantees JWT cache purging will happen
# - we want this to track resource consumption in the worst case

# And a more normal scenario where non-expiring JWTs are picked
# from an array
import time
import argparse
import random
import jwcrypto.jwt as jwt
from typing import Optional
from pathlib import Path

URL = "http://postgrest"

secret_key = "reallyreallyreallyreallyverysafe"


def generate_target(
    now: int,
    exp_inc: Optional[int],
    key: jwt.JWK,
) -> list[str]:
    """Generate a target using an HS256 or RS256 JWT"""
    headers = {
        "sub": f"user_{random.getrandbits(32)}",
        "iat": now,
    }

    if exp_inc is not None:
        headers["exp"] = now + exp_inc

    claims = {
        "role": "postgrest_test_author",
    }

    headers["alg"] = "RS256" if key.get("kty") == "RSA" else "HS256"

    token = jwt.JWT(headers, claims)
    token.make_signed_token(key)

    return [
        f"OPTIONS {URL}/authors_only?{headers["alg"]}",
        f"Authorization: Bearer {token.serialize()}",
        "",  # blank line to separate requests
    ]


def main():
    parser = argparse.ArgumentParser(
        description="Generate Vegeta targets with unique JWTs"
    )
    parser.add_argument(
        "generated_path",
        metavar="GENERATED_PATH",
        help="Path to write the generated files",
        type=Path,
    )
    parser.add_argument(
        "--private-key",
        dest="private_key_path",
        metavar="PRIVATE_KEY_PATH",
        type=Path,
        help="Path to the RSA private key file",
    )
    parser.add_argument(
        "--worst",
        dest="worst",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Generate worst case targets for a JWT cache",
    )

    args = parser.parse_args()

    targets_path = args.generated_path / "gen_targets.http"

    rsa_private_key: Optional[jwt.JWK] = None

    nsamples = 500  # per algorithm
    ntargets = 100000

    hs = jwt.JWK.from_password(secret_key)
    private_key_data = args.private_key_path.read_text()
    rsa = jwt.JWK.from_json(private_key_data)

    print(f"Generating {ntargets} targets...")

    now = int(time.time())

    lines = []

    # We want to ensure 401 Unauthorized responses don't happen during
    # JWT validation, this can happen when the jwt `exp` is too short.
    # At the same time, we want to ensure the `exp` is not too big,
    # so expires will occur and postgREST needs to
    # clean cached expired JWTs
    if args.worst:
        # estimated time it takes to run postgrest itself
        run_postgrest_time = 2

        for i in range(ntargets):
            target = generate_target(
                now, run_postgrest_time + i // 1000, rsa if i % 2 == 0 else hs
            )
            lines.extend(target)

    else:
        hs_targets = [generate_target(now, None, hs) for _ in range(nsamples)]
        rsa_targets = [generate_target(now, None, rsa) for _ in range(nsamples)]
        for i in range(ntargets):
            target = random.choice(hs_targets if i % 2 == 0 else rsa_targets)
            lines.extend(target)

    with open(targets_path, "w") as f:
        f.write("\n".join(lines))

    print(f"Created {ntargets} targets", end=" ")


if __name__ == "__main__":
    main()
