CORS
====

PostgREST sets highly permissive cross origin resource sharing, that is why it accepts Ajax requests from any domain.

It also handles `preflight requests <https://developer.mozilla.org/en-US/docs/Glossary/Preflight_request>`_ done by the browser, which are cached using the returned ``Access-Control-Max-Age: 86400`` header (86400 seconds = 24 hours). This is useful to reduce the latency of the subsequent requests.

A ``POST`` preflight request would look like this:

.. tabs::

  .. code-tab:: http

     OPTIONS /items HTTP/1.1
     Origin: http://example.com
     Access-Control-Allow-Method: POST
     Access-Control-Allow-Headers: Content-Type

  .. code-tab:: bash Curl

     curl -i "http://localhost:3000/items" \
       -X OPTIONS \
       -H "Origin: http://example.com" \
       -H "Access-Control-Request-Method: POST" \
       -H "Access-Control-Request-Headers: Content-Type"

.. code-block:: http

  HTTP/1.1 200 OK
  Access-Control-Allow-Origin: http://example.com
  Access-Control-Allow-Credentials: true
  Access-Control-Allow-Methods: GET, POST, PATCH, PUT, DELETE, OPTIONS, HEAD
  Access-Control-Allow-Headers: Authorization, Content-Type, Accept, Accept-Language, Content-Language
  Access-Control-Max-Age: 86400
