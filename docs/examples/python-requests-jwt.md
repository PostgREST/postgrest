## Python Client for PostgREST API

### Setup PostgreSQL

This code relies on setting up the PostgreSQL auth functions and grants correctly first. Follow [these instructions](http://postgrest.com/examples/users/).

After completing the PostgreSQL configuration, be sure to create a user with email, password, role, and verified flag. We'll use that user to login in the code below.

### Setup PostgREST

Next, setup PostgREST according to the documentation [http://postgrest.com/install/server/](here).

### Setup Python Client

Finally, we'll install and configure the python client. Follow the instructions in the [README](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/README.md). Be sure to set the [credentials](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/config.in#L3-L5) and [urls](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/config.in#L7-L9) in config.py.

### Python Client Functions

There are four primary functions to the python client:

* login
* construct_jwt_auth
* get_result_size
* get_range

The *login* and *construct_jwt_auth* functions will be required for any REST client using a PostgREST server, since a JWT auth instance is presumed.

The *get_result_size* and *get_range* functions are designed specifically for result sets where pagination is required. You can certainly use them for a single page result set that does not require pagination, but that may be overkill.

### Login
The [login function](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/client.py#L12-L17) takes email and password strings (credentials.email and credentials.password, respectively from the config.py) and return the response.

### Construct JWT Auth
The [construct_jwt_auth](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/client.py#L20-L23) function takes the auth response returned by the login function, retrieves the token in the response, and returns a JWT auth instance to the caller. The JWT auth instance can then be used for successive calls to the same PostgREST service.

### Get Result Size
The [get_result_size](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/client.py#L26-L30) function takes a JWT auth instance calls the URL at urls.data, extracts the size of the result set from the response object and returns the size.

### Get Range
The [get_range](https://github.com/davidthewatson/postgrest_python_requests_client/blob/master/client.py#L26-L30) function takes a beginning range, ending range, page size, and JWT auth instance, gets only that range of the available result set and returns JSON for that result set.
