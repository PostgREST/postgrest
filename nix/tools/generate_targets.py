# generates a file to be used by the vegeta load testing tool
import time
import argparse
import sys
import random
import jwt
import jwcrypto.jwk as jwk

SECRET = b"reallyreallyreallyreallyverysafe"
URL = "http://postgrest"
JWT_DURATION = 120
TOTAL_TARGETS = 50000  # tuned by hand to reduce result variance

key = jwk.JWK.generate(kty="RSA", size=4096)

private_key = jwt.algorithms.RSAAlgorithm.from_jwk(key.export_private())
public_key = key.export_public()


def generate_jwt() -> str:
    """Generate an HS256 JWT"""
    # Header & payload
    # header = {"alg": "HS256", "typ": "JWT"}
    now = int(time.time())
    payload = {
        "sub": f"user_{random.getrandbits(32)}",
        "iat": now,
        "role": "postgrest_test_author",
    }

    return jwt.encode(payload, private_key, "RS256")


# We want to ensure 401 Unauthorized responses don't happen during
# JWT validation, this can happen when the jwt `exp` is too short.
# At the same time, we want to ensure the `exp` is not too big,
# so expires will occur and postgREST will have to clean cached expired JWTs.
# def estimate_adequate_jwt_exp_increase(iteration: int) -> int:
#     # estimated time takes to build and run postgrest itself
#     build_run_postgrest_time = 2
#     # estimated time it takes to generate the targets file
#     file_generation_time = TOTAL_TARGETS // (10**-5)
#     # estimated exp time so some JWTs will expire
#     dynamic_exp_inc = iteration // 1000

#     return build_run_postgrest_time + file_generation_time + dynamic_exp_inc


def main():
    parser = argparse.ArgumentParser(
        description="Generate Vegeta targets with unique JWTs"
    )
    parser.add_argument(
        "output",
        help="Path to write the generated targets file",
    )
    parser.add_argument("jwk", help="Path to write the generated JWK")
    args = parser.parse_args()

    tokens = [generate_jwt() for _ in range(1000)]
    lines = []
    start_time = time.time()

    for i in range(TOTAL_TARGETS):
        token = random.choice(tokens)
        lines.append(f"OPTIONS {URL}/authors_only")
        lines.append(f"Authorization: Bearer {token}")
        lines.append("")  # blank line to separate requests

    try:
        with open(args.jwk, "w") as jwk, open(args.output, "w") as f:
            jwk.write(public_key)
            f.write("\n".join(lines))
    except IOError as e:
        print(f"Error writing to {args.output}: {e}", file=sys.stderr)
        sys.exit(1)

    elapsed = time.time() - start_time
    print(f"Created {TOTAL_TARGETS} targets with unique JWTs", end=" ")
    print(f"in {args.output} ({elapsed:.2f}s)")


if __name__ == "__main__":
    main()
