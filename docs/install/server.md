## Installation

### Installing from Pre-Built Release

The [release page](https://github.com/begriffs/postgrest/releases/latest) has precompiled binaries for Mac OS X and 64-bit Ubuntu. Next extract the tarball and run the binary inside with no arguments to see usage instructions:

```sh
# Untar the release (available at https://github.com/begriffs/postgrest/releases/latest)

$ tar zxf postgrest-0.2.12.0-osx.tar.xz

# Try running it
$ ./postgrest

# You should see a usage help message
```

<div class="admonition warning">
    <p class="admonition-title">Invitation to Contribute</p>

    <p>I currently build the binaries manually for each version. We need to set up an automated build matrix for various architectures. It should support 32- and 64-bit versions of

    <ul><li>Scientific Linux 6</li><li>CentOS</li><li>RHEL 6</li></ul>

    Also it would be good to create packages for Homebrew, and apt.</p>
</div>

We'll learn the meaning of the command line flags later, but here is a minimal example of running the app. It does all operations as user `postgres`, including for unauthenticated requests.

```sh
$ ./postgrest -d dbname -U postgres --a postgres --v1schema public
```

### Building from Source

When a prebuilt binary does not exist for your system you can build the project from source. You'll also need to do this if you want to help with development. [Stack](https://github.com/commercialhaskell/stack) makes it easy. It will install any necessary Haskell dependencies on your system.

* [Install Stack](https://github.com/commercialhaskell/stack#how-to-install) for your platform
* Build the project

```bash
git clone https://github.com/begriffs/postgrest.git
cd postgrest
stack build
```

* Run the server

```bash
stack exec postgrest -- arg1 arg2
# ... your arguments after the double dashes
```

If you want to run the test suite, stack can do that too: `stack test`.

### Installing PostgreSQL

To use PostgREST you will need an underlying database. You can use something like Amazon [RDS](https://aws.amazon.com/rds/) but installing your own locally is cheaper and more convenient for development.

* [Instructions for OS X](http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/)
* [Instructions for Ubuntu 14.04](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04)

