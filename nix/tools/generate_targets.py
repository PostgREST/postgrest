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
import subprocess
import sys
import random
import jwt
from typing import Optional
from pathlib import Path
from enum import Enum

URL = "http://postgrest"

secret_key = b"reallyreallyreallyreallyverysafe"


def generate_jwt(
    now: int,
    exp_inc: Optional[int],
    rsa_private_key: Optional[jwt.algorithms.RSAAlgorithm],
) -> str:
    """Generate an HS256 or RS256 JWT"""
    payload = {
        "sub": f"user_{random.getrandbits(32)}",
        "iat": now,
        "role": "postgrest_test_author",
    }

    if exp_inc is not None:
        payload["exp"] = now + exp_inc

    if rsa_private_key is None:
        key = secret_key
        alg = "HS256"
    else:
        key = rsa_private_key
        alg = "RS256"
    return jwt.encode(payload, key, alg)


HTTP_METHODS = (
    "GET",
    "OPTIONS",
)

HttpMethod = Enum(
    "HttpMethod",
    {method: method for method in HTTP_METHODS},
    type=str,
    module=__name__,
)


def append_targets(lines: list[str], token: str, http_method: HttpMethod):
    lines.append(f"{http_method.value} {URL}/authors_only")
    lines.append(f"Authorization: Bearer {token}")
    lines.append("")  # blank line to separate requests


# we use this to chain commands on loadtest.nix
def run_command(command: list[str]):
    if not command:
        return

    if command[0] == "--":
        command = command[1:]

    if not command:
        return

    try:
        subprocess.run(command, check=True)
    except subprocess.CalledProcessError as exc:
        print(
            f"Error executing command {' '.join(command)}: {exc}",
            file=sys.stderr,
        )
        sys.exit(exc.returncode)


def main():
    parser = argparse.ArgumentParser(
        description="Generate Vegeta targets with unique JWTs"
    )
    parser.add_argument(
        "targets_path",
        metavar="TARGETS_PATH",
        help="Path to write the generated targets file",
    )
    parser.add_argument(
        "--private-key",
        dest="private_key_path",
        metavar="PRIVATE_KEY_PATH",
        type=Path,
        default=None,
        help="Path to the RSA private key file (required when --rsa is used)",
    )
    parser.add_argument(
        "--worst",
        dest="worst",
        action=argparse.BooleanOptionalAction,
        default=False,
        help="Generate worst case targets for a JWT cache",
    )
    parser.add_argument(
        "--rsa",
        dest="jwk_path",
        metavar="JWK_PATH",
        type=Path,
        default=None,
        help="Path to an existing RSA JWK file used for signing tokens",
    )
    parser.add_argument(
        "--method",
        dest="http_method",
        choices=list(HTTP_METHODS),
        required=True,
        help="HTTP method for the vegeta targets",
    )
    parser.add_argument(
        "command",
        nargs=argparse.REMAINDER,
        help="Command (and arguments) to run after generating the targets",
    )

    args = parser.parse_args()

    rsa_private_key: Optional[jwt.algorithms.RSAAlgorithm] = None

    is_hs = args.jwk_path is None

    http_method = HttpMethod(args.http_method)

    nsamples = 1000

    if is_hs:
        ntargets = 200000
    else:
        # The asymmetric targets take too long to compute so we reduce them
        ntargets = 50000

    if not is_hs:
        if args.private_key_path is None:
            parser.error("--rsa requires the --private-key option")
        try:
            private_key_data = args.private_key_path.read_text()
        except OSError as e:
            err = (
                f"Error reading RSA private key from {args.private_key_path}: "
                f"{e}. Generate RSA materials first with gen_rsa_materials.py."
            )
            print(err, file=sys.stderr)
            sys.exit(1)

        try:
            rsa_private_key = jwt.algorithms.RSAAlgorithm.from_jwk(private_key_data)
        except Exception as exc:  # broad exception to capture parsing errors
            err = (
                f"Error loading RSA private key from {args.private_key_path}: " f"{exc}"
            )
            print(err, file=sys.stderr)
            sys.exit(1)

    print(f"Generating {ntargets} targets...")

    start_time = time.time()

    now = int(start_time)

    lines = []

    # We want to ensure 401 Unauthorized responses don't happen during
    # JWT validation, this can happen when the jwt `exp` is too short.
    # At the same time, we want to ensure the `exp` is not too big,
    # so expires will occur and postgREST needs to
    # clean cached expired JWTs
    if args.worst:
        # estimated time takes to build and run postgrest itself
        build_run_postgrest_time = 2

        # estimated time it takes to generate the targets file
        # the division numbers are tuned by hand
        if is_hs:  # hs generation is much faster
            gen_time = ntargets // 66666
        else:  # asymmetric is slower so the time is higher
            gen_time = ntargets // 220

        # estimated exp time so some JWTs will expire
        inc = build_run_postgrest_time + gen_time

        for i in range(ntargets):
            token = generate_jwt(now, inc + i // 1000, rsa_private_key)
            append_targets(lines, token, http_method)

    else:
        tokens = [generate_jwt(now, None, rsa_private_key) for _ in range(nsamples)]
        for i in range(ntargets):
            token = random.choice(tokens)
            append_targets(lines, token, http_method)

    try:
        with open(args.targets_path, "w") as f:
            f.write("\n".join(lines))
    except IOError as e:
        print(f"Error writing to {args.targets_path}: {e}", file=sys.stderr)
        sys.exit(1)

    elapsed = time.time() - start_time
    print(f"Created {ntargets} targets", end=" ")
    print(f"in {args.targets_path} ({elapsed:.2f}s)")

    run_command(args.command)


if __name__ == "__main__":
    main()
