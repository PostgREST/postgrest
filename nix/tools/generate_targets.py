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
import jwcrypto.jwk as jwk
from typing import Optional
from pathlib import Path

URL = "http://postgrest"

secret_key = b"reallyreallyreallyreallyverysafe"

key = jwk.JWK.generate(kty="RSA", size=4096)
private_key = jwt.algorithms.RSAAlgorithm.from_jwk(key.export_private())
public_key = key.export_public()


def generate_jwt(now: int, exp_inc: Optional[int], is_hs: bool) -> str:
    """Generate an HS256 or RS256 JWT"""
    payload = {
        "sub": f"user_{random.getrandbits(32)}",
        "iat": now,
        "role": "postgrest_test_author",
    }

    if exp_inc is not None:
        payload["exp"] = now + exp_inc

    k = secret_key if is_hs else private_key
    alg = "HS256" if is_hs else "RS256"
    return jwt.encode(payload, k, alg)


def append_targets(lines: list[str], token: str):
    lines.append(f"OPTIONS {URL}/authors_only")
    lines.append(f"Authorization: Bearer {token}")
    lines.append("")  # blank line to separate requests


def main():
    parser = argparse.ArgumentParser(
        description="Generate Vegeta targets with unique JWTs"
    )
    parser.add_argument(
        "output",
        help="Path to write the generated targets file",
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
        help="Path for generating a RSA JWK file to sign tokens with",
    )

    args = parser.parse_args()

    is_hs = args.jwk_path is None

    nsamples = 1000
    if is_hs:
        ntargets = 200000
    else:
        # The asymmetric targets take too long to compute so we reduce them
        ntargets = 50000

    if not is_hs:
        try:
            with open(args.jwk_path, "w") as jwk:
                jwk.write(public_key)
                print(f"Created {args.jwk_path} file containing the RSA JWK")
        except IOError as e:
            print(f"Error writing to {args.jwk_path}: {e}", file=sys.stderr)
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
            token = generate_jwt(now, inc + i // 1000, is_hs)
            append_targets(lines, token)

    else:
        tokens = [generate_jwt(now, None, is_hs) for _ in range(nsamples)]
        for i in range(ntargets):
            token = random.choice(tokens)
            append_targets(lines, token)

    try:
        with open(args.output, "w") as f:
            f.write("\n".join(lines))
    except IOError as e:
        print(f"Error writing to {args.output}: {e}", file=sys.stderr)
        sys.exit(1)

    elapsed = time.time() - start_time
    print(f"Created {ntargets} targets", end=" ")
    print(f"in {args.output} ({elapsed:.2f}s)")


if __name__ == "__main__":
    main()
