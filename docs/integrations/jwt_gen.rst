.. _external_jwt:

External JWT Generation
-----------------------

JWT from Auth0
~~~~~~~~~~~~~~

An external service like `Auth0 <https://auth0.com/>`_ can do the hard work transforming OAuth from Github, Twitter, Google etc into a JWT suitable for PostgREST. Auth0 can also handle email signup and password reset flows.

To use Auth0, create `an application <https://auth0.com/docs/get-started/applications>`_ for your app and `an API <https://auth0.com/docs/get-started/apis>`_ for your PostgREST server. Auth0 supports both HS256 and RS256 scheme for the issued tokens for APIs. For simplicity, you may first try HS256 scheme while creating your API on Auth0. Your application should use your PostgREST API's `API identifier <https://auth0.com/docs/get-started/apis/api-settings>`_ by setting it with the `audience parameter <https://auth0.com/docs/secure/tokens/access-tokens/get-access-tokens#control-access-token-audience>`_  during the authorization request. This will ensure that Auth0 will issue an access token for your PostgREST API. For PostgREST to verify the access token, you will need to set ``jwt-secret`` on PostgREST config file with your API's signing secret.

.. note::

  Our code requires a database role in the JWT. To add it you need to save the database role in Auth0 `app metadata <https://auth0.com/docs/manage-users/user-accounts/metadata/manage-metadata-rules>`_. Then, you will need to write `a rule <https://auth0.com/docs/customize/rules>`_ that will extract the role from the user's app_metadata and set it as a `custom claim <https://auth0.com/docs/get-started/apis/scopes/sample-use-cases-scopes-and-claims#add-custom-claims-to-a-token>`_ in the access token. Note that, you may use Auth0's `core authorization feature <https://auth0.com/docs/manage-users/access-control/rbac>`_ for more complex use cases. Metadata solution is mentioned here for simplicity.

  .. code:: javascript

    function (user, context, callback) {

      // Follow the documentations at
      // https://postgrest.org/en/latest/configuration.html#db-role-claim-key
      // to set a custom role claim on PostgREST
      // and use it as custom claim attribute in this rule
      const myRoleClaim = 'https://myapp.com/role';

      user.app_metadata = user.app_metadata || {};
      context.accessToken[myRoleClaim] = user.app_metadata.role;
      callback(null, user, context);
    }

