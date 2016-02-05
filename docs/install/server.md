## Installation

### Installing from Pre-Built Release

The [release page](https://github.com/begriffs/postgrest/releases/latest)
has precompiled binaries for Mac OS X, Windows, and several Linux
distros.  Extract the tarball and run the binary inside with no
arguments to see usage instructions:

```sh
# Untar the release (available at https://github.com/begriffs/postgrest/releases/latest)

$ tar zxf postgrest-[version]-[platform].tar.xz

# Try running it
$ ./postgrest

# You should see a usage help message
```

<div class="admonition warning">
    <p class="admonition-title">Invitation to Contribute</p>

    <p>I currently build the binaries manually for each architecture.
    It would be nice to set up an automated build matrix for various
    architectures. It should support Mac, Windows and 32- and 64-bit
    versions of

    <ul><li>Scientific Linux 6</li><li>CentOS</li><li>RHEL 6</li></ul></p>
</div>

### Building from Source

When a prebuilt binary does not exist for your system you can build
the project from source. You'll also need to do this if you want
to help with development.
[Stack](https://github.com/commercialhaskell/stack) makes it easy.
It will install any necessary Haskell dependencies on your system.

* [Install Stack](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform
```bash
#ubuntu example
#See the link above for other operating systems

wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y
```
* Install libpq-dev
```
sudo apt-get install -y libpq-dev
```
* Build & install in one step

```bash
git clone https://github.com/begriffs/postgrest.git
cd postgrest
sudo stack install --install-ghc --local-bin-path /usr/local/bin
```

* Run the server

If you want to run the test suite, stack can do that too: `stack test`.

### Running the Server

```bash
postgrest postgres://user:pass@host:port/db -a anon_user [other flags]
```

The user in the connection string is the "authenticator role," i.e.
a role which is used temporarily to switch into other roles depending
on the authentication request JWT. For simple API's you can use the
same role for authenticator and anonymous.

The complete list of options:

<dl>
<dt>-p, --port</dt>
<dd>The port on which the server will listen for HTTP requests.
    Defaults to 3000.</dd>

<dt>-a, --anonymous (required)</dt>
<dd>The database role used to execute commands for those requests
    which provide no JWT authorization.</dd>

<dt>-s, --schema</dt>
<dd>The db schema which you want to expose as an API. For historical
    reasons it defaults to <code>1</code>, but you're more likely
    to want to choose a value of <code>public</code>.</dd>

<dt>-j, --jwt-secret</dt>
<dd>The secret passphrase used to encrypt JWT tokens. Defaults to
    <code>secret</code> but do not use the default in production!
    Load-balanced PostgREST servers should share the same secret.</dd>

<dt>-p, --pool</dt>
<dd>Max connections to use in db pool. Defaults to to 10, but you
    should find an optimal value for your db by running the SQL
    command <code>show max_connections;</code></dd>

<dt>-m, --max-rows</dt>
<dd>Max number of rows to return in a read request. The default is
    no limit.</dd>
</dl>

<div class="admonition note">
    <p class="admonition-title">Hiding Password from Process List</p>

    <p>Passing the database password and JWT secret as naked
    parameters might not be a good idea because the parameters are
    visible in a <code>ps</code> listing. One solution is to set
    environment variables such as PASS and use <code>$PASS</code>
    in the connection string.  Another is to use a user-specific
    <a
    href="http://www.postgresql.org/docs/current/static/libpq-pgpass.html">.pgpass</a>
    file.</p>
</div>

When running `postgrest` on the same machine as PostgreSQL, it is also
possible to connect to the database using the [Unix socket]
(https://en.wikipedia.org/wiki/Unix_domain_socket) and the
[Peer Authentication method]
(http://www.postgresql.org/docs/current/static/auth-methods.html#AUTH-PEER)
as an alternative to TCP/IP communication and authentication with a password.

The Peer Authentication grants access to the database to any Unix user
who connects as a user of the same name in the database.
Since the empty host resolves to the Unix socket]
(http://www.postgresql.org/docs/current/static/libpq-connect.html#AEN42494)
and the password can be omitted in this case,
the command line is reduced to:

```sh
sudo -u user postgrest postgres://user@/db [flags]
```

where the `sudo -u user` command runs the following command as given `user`.

If you create a Unix user `postgrest` and a database user `postgrest`
for example, the command becomes:

```sh
sudo -u postgrest postgrest postgres://postgrest@/db [flags]
```

The first `postgrest` is the Unix user name, the second `postgrest`
is the name of the executable, the third `postgrest` is the name
of the database user.

### Install via Homebrew (Mac OS X)

You can use the Homebrew package manager to install PostgREST on Mac

```bash
# Ensure brew is up to date
brew update

# Check for any problems with brew's setup
brew doctor

# Install the postgrest package
brew install postgrest
```

This will automatically install PostgreSQL as a dependency (see the [Installing PostgreSQL](#installing-postgresql) section for setup instructions). The process tends to take up to 15 minutes to install the package and its dependencies.

After installation completes, the tool is added to your $PATH and can be used from anywhere with:

```bash
postgrest --help
```

### Installing PostgreSQL

To use PostgREST you will need an underlying database (PostgreSQL version 9.3 or greater is required). You can use something like Amazon [RDS](https://aws.amazon.com/rds/) but installing your own locally is cheaper and more convenient for development.

* [Instructions for OS X](http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/)
* [Instructions for Ubuntu 14.04](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04)
* [Installer for Windows](http://www.enterprisedb.com/products-services-training/pgdownload#windows)
