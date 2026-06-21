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
import sys
import random
import jwt
from typing import Optional
from pathlib import Path

URL = "http://postgrest"

secret_key = b"reallyreallyreallyreallyverysafe"


def generate_target(
    now: int,
    exp_inc: Optional[int],
    rsa_private_key: Optional[jwt.algorithms.RSAAlgorithm],
) -> list[str]:
    """Generate a target using an HS256 or RS256 JWT"""
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
    token = jwt.encode(payload, key, alg)

    return [
        f"OPTIONS {URL}/authors_only?{alg}",
        f"Authorization: Bearer {token}",
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

    rsa_private_key: Optional[jwt.algorithms.RSAAlgorithm] = None

    nsamples = 500  # per algorithm
    ntargets = 100000

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
        err = f"Error loading RSA private key from {args.private_key_path}: " f"{exc}"
        print(err, file=sys.stderr)
        sys.exit(1)

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
                now,
                run_postgrest_time + i // 1000,
                rsa_private_key if i % 2 == 0 else None,
            )
            lines.extend(target)

    else:
        hs_targets = [generate_target(now, None, None) for _ in range(nsamples)]
        rsa_targets = [
            generate_target(now, None, rsa_private_key) for _ in range(nsamples)
        ]
        for i in range(ntargets):
            target = random.choice(hs_targets if i % 2 == 0 else rsa_targets)
            lines.extend(target)

    try:
        with open(targets_path, "w") as f:
            f.write("\n".join(lines))
    except IOError as e:
        print(f"Error writing to {targets_path}: {e}", file=sys.stderr)
        sys.exit(1)

    print(f"Created {ntargets} targets", end=" ")


if __name__ == "__main__":
    main()
