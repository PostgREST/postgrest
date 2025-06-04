# generates a file to be used by the vegeta load testing tool
import time
import argparse
import sys
import random
import jwt
import jwcrypto.jwk as jwk

URL = "http://postgrest"
TOTAL_JWTS = 1000
TOTAL_TARGETS = 50000  # tuned by hand to reduce result variance

key = jwk.JWK.generate(kty="RSA", size=4096)

private_key = jwt.algorithms.RSAAlgorithm.from_jwk(key.export_private())
public_key = key.export_public()


def generate_jwt() -> str:
    """Generate an RS256 JWT"""
    now = int(time.time())
    payload = {
        "sub": f"user_{random.getrandbits(32)}",
        "iat": now,
        "role": "postgrest_test_author",
    }

    return jwt.encode(payload, private_key, "RS256")


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

    tokens = [generate_jwt() for _ in range(TOTAL_JWTS)]
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
