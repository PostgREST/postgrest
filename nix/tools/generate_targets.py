# generates a file to be used by the vegeta load testing tool
import time
import hmac
import hashlib
import base64
import json
import argparse
import sys

SECRET = b"reallyreallyreallyreallyverysafe"
URL = "http://postgrest"
JWT_DURATION = 120
TOTAL_TARGETS = 200_000  # tuned by hand to reduce result variance


def base64url_encode(data: bytes) -> str:
    """URL-safe Base64 encode without padding."""
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode("ascii")


def generate_jwt() -> str:
    """Generate an HS256 JWT"""
    # Header & payload
    header = {"alg": "HS256", "typ": "JWT"}
    now = int(time.time())
    payload = {
        "iat": now,
        "exp": now + JWT_DURATION,
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
    for _ in range(TOTAL_TARGETS):
        token = generate_jwt()
        lines.append(f"OPTIONS {URL}/authors_only")
        lines.append(f"Authorization: Bearer {token}")
        lines.append("")  # blank line to separate requests

    try:
        with open(args.output, "w") as f:
            f.write("\n".join(lines))
    except IOError as e:
        print(f"Error writing to {args.output}: {e}", file=sys.stderr)
        sys.exit(1)

    print(f"Generated {TOTAL_TARGETS} targets in {args.output}")


if __name__ == "__main__":
    main()
