## External Authentication

API clients authenticate with [JSON Web Tokens](http://jwt.io).
PostgREST does not support any other authentication mechanism
directly, but they can be built on top. In this demo we will build
a system that works with an external authentication server
and integrates with a PostgREST server by sharing the same JWT secret.

For a better understanding of JWT and PostgREST authentication system you should read
the [User Management](users/) example as well.

I'll use a [Rails](http://rubyonrails.org) application using [Devise](https://github.com/plataformatec/devise)
just to make the example more concrete, but this could be replicated for
any other external authentication system using the same principles.
In case Rails is not your cup of tea you can continue reading and
just skip the Ruby code samples. I'll also assume
the use of JQuery for some client-side code samples for the sake of simplicity.

I won't delve into Devise authentication details, for this would require a tutorial on its own,
so I'm assuming that the reader's authentication system is already working.

### Sharing the JWT Secret

Allowing a third party to generate valid JWTs for your PostgREST API
is just a matter of sharing a secret. So you need to give your authenticator
software the same secret that was used in your API server under the ```--jwt-secret```
parameter.

This could be done easly using environment variables. You set a ```JWT_SECRET``` variable
in the environment where you run your rails app and it will be accessible in the global
variable ```ENV['JWT_SECRET']```.

### User Model

We will map each user in this example to two database roles.
So our application users are either ```admin``` or ```customer```.
If they are just visitors (not logged in) to our website they will be ```anonymous```.
One way of mapping users is to add a field in our users table indicating their database role.
I'll add a text field called role to my users table:

```sql
ALTER TABLE users ADD role text NOT NULL DEFAULT 'customer';
```

Besides the main user that PostgREST uses to connect to PostgreSQL
and the anonymous user, we will need two aditional roles for our example:

* admin - to be used by users that access all the system rows.
* customer - to be used when user has restricted access to database rows.

Bellow we have the commands to create all roles that will be used:
```sql
CREATE USER authenticator NOINHERIT;
CREATE ROLE anonymous;
CREATE ROLE admin;
CREATE ROLE customer;

GRANT customer, admin, anonymous TO authenticator;
```

### Generating a JWT

Several libraries are available to generate JWT, you will find a very handy list in [their website](http://jwt.io)
under **Libraries**.
To continue our Rails example I'll use the ruby library [json_web_token](https://github.com/garyf/json_web_token).

In order to make the gem available in my Rails project I add the following line to my Gemfile:

```
gem 'json_web_token'
```

Then we create a Rails controller to serve JWTs for my authenticated users.
For this I just open a file ```app/controllers/api_tokens_controller.rb``` with the content:

```ruby
class ApiTokensController < ApplicationController
  TOKEN_TTL = 1.hour

  def show
    unless ENV['JWT_SECRET'].present?
      return render json: {error: "you need to have JWT_SECRET configured to get an API token"}, status: 500
    end

    unless current_user.present?
      return render json: {error: "only authenticated users can request the API token"}, status: 401
    end

    expires_in TOKEN_TTL, public: false
    render json: {token: jwt}, status: 200
  end

  private
  def jwt
    JsonWebToken.sign(claims, key: ENV['JWT_SECRET'])
  end

  def claims
    # This token will expire 1 hour after being issued
    {
      role: current_user.role,
      user_id: current_user.id.to_s,
      exp: (Time.now + TOKEN_TTL).to_i
    }
  end
end
```

<div class="admonition note">
    <p class="admonition-title">Token Time to Live</p>
    <p>
    In the code above we leverage the HTTP time based cache headers to expire the
    endpoint cache at the same time as the token. In this example we have a token
    that will be refresh one hour after its issuing time.
    That's why both are based on the <code>TOKEN_TTL</code> constant.
    </p>
</div>

We also need to create a route in the ```config/routes.rb``` file:

```ruby
resource :api_token, only: [:show]
```

Now, any authenticated user in our rails application can request an api_token making a GET
request to ```/api_token```. This endpoint will return a json object with one property
whose value is the token the API requests should use.

### Orders Endpoint

Here is how to create a view to generate an endpoint ```/orders``` filtered by
the logged in user:

```sql
ALTER DATABASE mydb SET postgrest.claims.user_id TO '';

CREATE OR REPLACE FUNCTION current_user_id()
RETURNS integer
STABLE
LANGUAGE SQL
AS $$
    SELECT nullif(current_setting('postgrest.claims.user_id'), '')::integer;
$$;

CREATE SCHEMA private;

CREATE TABLE private.orders (
    id serial primary key,
    user_id int references users,
    created_at timestamp not null default current_timestamp,
    updated_at timestamp not null default current_timestamp
);

CREATE VIEW orders AS
SELECT
    id, user_id, created_at, updated_at
FROM
    private.orders o
WHERE
    current_user = 'admin' OR o.user_id = current_user_id();
```

<div class="admonition note">
    <p class="admonition-title">DRY priviledge checking conditions</p>
    <p>
    You can encapsulate conditions that will be commonly used to check for privileges while reading a database row.
    We used a function <code>current_user_id()</code> but we could add more conditions to functions
    as the system becomes more complex.<br/>
    Remeber to mark your functions as <code>STABLE</code> so that PostgreSQL can inline then while planning the query.
    </p>
</div>

### Using the JWT

Now whenever you are authenticated in your Rails application you can use some Javascript
 code to get the token and use it:
```javascript
$.getJSON('/api_token').done(function(data){
    $.ajax('/orders', {'Authorization': 'Bearer ' + data.token}).done(function(data){
        console.log('Visible Orders: ', data);
    })
}).fail(function(){
    console.log('Error fetching API token');
})
```
We could also store the token to avoid having to fetch it again in the same page.

### Conclusion

This section explained the implementation details for building an
external authentication system working with PostgREST.
With the previous [User Management](users/) example this should give a clearer
idea of how to set up authentication for your API.
