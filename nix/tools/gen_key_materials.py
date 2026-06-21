# Generate HS & RSA JWK/public material for loadtests.

import argparse
import sys
from pathlib import Path

import jwcrypto.jwk as jwk


def main():
    parser = argparse.ArgumentParser(
        description="Generate RSA JWK/private key pair for loadtests"
    )
    parser.add_argument(
        "--jwks",
        dest="jwks_path",
        metavar="JWKS_PATH",
        type=Path,
        required=True,
        help="Path to write the JWKS file",
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

    hs = jwk.JWK.from_password("reallyreallyreallyreallyverysafe")
    rsa = jwk.JWK.generate(kty="RSA", size=4096)

    jwks = jwk.JWKSet()
    jwks.add(hs)
    jwks.add(rsa)

    # Technically, this exports the private keys, because HS does not have the concept
    # of a public key. This is not a problem for tests, though, PostgREST can verify
    # tokens with the private key just as well.
    args.jwks_path.write_text(jwks.export())
    print(f"Created JWKSet on {args.jwks_path}")

    args.private_key_path.write_text(rsa.export_private())
    print(f"Created private key on {args.private_key_path}")


if __name__ == "__main__":
    main()
