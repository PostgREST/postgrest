# generates a file to be used by the vegeta load testing tool
# it generates TOTAL_TARGETS amount of requests that will be run

# This is a worst case scenario for the JWT cache:
# - all requests will have a unique JWT so no cache hits
# - all jwts have an expiration that will be long enough to be
#   valid at time of request but short enough that already
#   validated jwts will expire later during the loadtest run
# - the above guarantees JWT cache purging will happen
#
# We want this to track resource consumption in the worst case
import time
import hmac
import hashlib
import base64
import json
import argparse
import sys
import random

SECRET = b"reallyreallyreallyreallyverysafe"
URL = "http://postgrest"
TOTAL_TARGETS = 200000  # tuned by hand to reduce result variance


def base64url_encode(data: bytes) -> str:
    """URL-safe Base64 encode without padding."""
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode("ascii")


def generate_jwt(exp_inc: int) -> str:
    """Generate an HS256 JWT"""
    # Header & payload
    header = {"alg": "HS256", "typ": "JWT"}
    now = int(time.time())
    payload = {
        "sub": f"user_{random.getrandbits(32)}",
        "iat": now,
        "exp": now + exp_inc,
        "role": "postgrest_test_author",
    }

    # Encode to JSON and then to Base64URL
    header_b = json.dumps(header, separators=(",", ":")).encode()
    payload_b = json.dumps(payload, separators=(",", ":")).encode()
    header_b64 = base64url_encode(header_b)
    payload_b64 = base64url_encode(payload_b)

    # Sign (HMAC‑SHA256) the "<header>.<payload>" string
    signing_input = f"{header_b64}.{payload_b64}".encode()
    signature = hmac.new(SECRET, signing_input, hashlib.sha256).digest()
    signature_b64 = base64url_encode(signature)

    return f"{header_b64}.{payload_b64}.{signature_b64}"


# We want to ensure 401 Unauthorized responses don't happen during
# JWT validation, this can happen when the jwt `exp` is too short.
# At the same time, we want to ensure the `exp` is not too big,
# so expires will occur and postgREST will have to clean cached expired JWTs.
def estimate_adequate_jwt_exp_increase(iteration: int) -> int:
    # estimated time takes to build and run postgrest itself
    build_run_postgrest_time = 2
    # estimated time it takes to generate the targets file
    file_generation_time = TOTAL_TARGETS // (10**-5)
    # estimated exp time so some JWTs will expire
    dynamic_exp_inc = iteration // 1000

    return build_run_postgrest_time + file_generation_time + dynamic_exp_inc


def main():
    parser = argparse.ArgumentParser(
        description="Generate Vegeta targets with unique JWTs"
    )
    parser.add_argument(
        "output",
        help="Path to write the generated targets file",
    )
    args = parser.parse_args()

    lines = []
    start_time = time.time()

    for i in range(TOTAL_TARGETS):
        token = generate_jwt(estimate_adequate_jwt_exp_increase(i))
        lines.append(f"OPTIONS {URL}/authors_only")
        lines.append(f"Authorization: Bearer {token}")
        lines.append("")  # blank line to separate requests

    try:
        with open(args.output, "w") as f:
            f.write("\n".join(lines))
    except IOError as e:
        print(f"Error writing to {args.output}: {e}", file=sys.stderr)
        sys.exit(1)

    elapsed = time.time() - start_time
    print(f"Created {TOTAL_TARGETS} targets with unique JWTs", end=" ")
    print(f"in {args.output} ({elapsed:.2f}s)")


if __name__ == "__main__":
    main()
