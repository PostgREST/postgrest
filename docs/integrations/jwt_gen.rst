.. _external_jwt:

External JWT Generation
-----------------------

JWT from Auth0
~~~~~~~~~~~~~~

An external service like `Auth0 <https://auth0.com/>`_ can do the hard work transforming OAuth from Github, Twitter, Google etc into a JWT suitable for PostgREST. Auth0 can also handle email signup and password reset flows.

To use Auth0, create `an application <https://auth0.com/docs/get-started/applications>`_ for your app and `an API <https://auth0.com/docs/get-started/apis>`_ for your PostgREST server. Auth0 supports both HS256 and RS256 scheme for the issued tokens for APIs. For simplicity, you may first try HS256 scheme while creating your API on Auth0. Your application should use your PostgREST API's `API identifier <https://auth0.com/docs/get-started/apis/api-settings>`_ by setting it with the `audience parameter <https://auth0.com/docs/secure/tokens/access-tokens/get-access-tokens#control-access-token-audience>`_  during the authorization request. This will ensure that Auth0 will issue an access token for your PostgREST API. For PostgREST to verify the access token, you will need to set ``jwt-secret`` on PostgREST config file with your API's signing secret.

JWT using OpenSSL
~~~~~~~~~~~~~~~~~

To manually generate a JWT using ``openssl`` commands, you can use the following script. This may be useful for testing JWT related features of PostgREST.

.. code:: bash

  #!/bin/bash
  set -e

  TEST_JWT_SECRET='test_secret_that_is_at_least_32_characters_long'
  _base64 () { openssl base64 -e -A | tr '+/' '-_' | tr -d '='; }
  header=$(echo -n '{"alg":"HS256","typ":"JWT"}' | _base64)
  exp=$(( EPOCHSECONDS + 60*60 ))  # 1 hour
  payload=$(echo -n "{\"role\":\"test_role\",\"exp\":$exp}" | _base64)
  signature=$(echo -n "$header.$payload" | openssl dgst -sha256 -hmac "$TEST_JWT_SECRET" -binary | _base64)
  echo -n "$header.$payload.$signature"
