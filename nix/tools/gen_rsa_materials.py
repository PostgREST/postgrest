# Generate RSA JWK/public material for loadtests.

import argparse
import sys
from pathlib import Path

import jwcrypto.jwk as jwk


def main():
    parser = argparse.ArgumentParser(
        description="Generate RSA JWK/private key pair for loadtests"
    )
    parser.add_argument(
        "--rsa",
        dest="jwk_path",
        metavar="JWK_PATH",
        type=Path,
        required=True,
        help="Path to write the RSA JWK file",
    )
    parser.add_argument(
        "--private-key",
        dest="private_key_path",
        metavar="PRIVATE_KEY_PATH",
        type=Path,
        required=True,
        help="Path to write the RSA private key file",
    )

    args = parser.parse_args()

    key = jwk.JWK.generate(kty="RSA", size=4096)
    private_jwk, public_jwk = key.export_private(), key.export_public()

    try:
        args.jwk_path.write_text(public_jwk)
        print(f"Created RSA JWK on {args.jwk_path}")
    except OSError as e:
        print(f"Error writing to {args.jwk_path}:{e}", file=sys.stderr)
        sys.exit(1)

    try:
        args.private_key_path.write_text(private_jwk)
        print(f"Created private key on {args.private_key_path}")
    except OSError as e:
        print(f"Error writing to {args.private_key_path}:{e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
