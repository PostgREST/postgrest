## Accessing PostgREST API using Python Requests and Requests-JWT

This code relies on setting up the PostgreSQL auth functions and grants correctly first. Follow these instructions:

http://postgrest.com/examples/users/

After completing the configuration, be sure to create a user with email, password, role, and verified flag. We'll use that user to login in the code below.

Install the required libraries

    pip install requests
    pip install requests-jwt

Then, from a python interpreter or script:

    import requests
    import requests_jwt
    import json

    # email and password auth through postgrest
    resp = requests.post('http://localhost:3000/rpc/login', json={"email": "you@yours.com", "pass": "dog"})
    if resp.status_code != 200:
        raise Exception()

    # JWT auth using the token above
    token = json.loads(resp.text)['token']
    auth = requests_jwt.JWTAuth(token)
    r = requests.get('http://localhost:3000/weight', auth=auth)
    if r.status_code != 200:
        raise Exception()

    for each in r.json():
        # do as you wish with each row
        print(each)

The preceding HTTP client example should work on Python 3.x. If you're using python < 3.x you may need to change the print functions to statements or use:

    from __future__ import print_function
