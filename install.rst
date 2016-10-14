Binary Release
==============

The `release page <https://github.com/begriffs/postgrest/releases/latest>`_ has precompiled binaries for Mac OS X, Windows, and several Linux distros. Extract the tarball and run the binary inside with the :code:`--help` flag to see usage instructions:

.. code-block:: bash

  # Untar the release (available at https://github.com/begriffs/postgrest/releases/latest)

  $ tar zxf postgrest-[version]-[platform].tar.xz

  # Try running it
  $ ./postgrest --help

  # You should see a usage help message

Build from Source
=================

.. note::

  We discourage building and using PostgREST on **Alpine Linux** because of a reported GHC memory leak on that platform.

When a prebuilt binary does not exist for your system you can build the project from source. You'll also need to do this if you want to help with development. `Stack <https://github.com/commercialhaskell/stack>`_ makes it easy. It will install any necessary Haskell dependencies on your system.

* `Install Stack <http://docs.haskellstack.org/en/stable/README.html#how-to-install>`_ for your platform
* Install Library Dependencies

  =====================  ============================
  Operating System       Dependencies
  =====================  ============================
  Ubuntu/Debian          libpq-dev
  CentOS/Fedora/Red Hat  postgresql-devel, zlib-devel
  BSD                    postgresql95-server
  =====================  ============================

* Build and install binary

  .. code-block:: bash

    git clone https://github.com/begriffs/postgrest.git
    cd postgrest
    stack build --install-ghc
    sudo stack install --allow-different-user --local-bin-path /usr/local/bin

* Check that the server is installed: :code:`postgrest --help`.

If you want to run the test suite, stack can do that too: :code:`stack test`.

PostgreSQL dependency
=====================

To use PostgREST you will need an underlying database (PostgreSQL version 9.3 or greater is required). You can use something like Amazon `RDS <https://aws.amazon.com/rds/>`_ but installing your own locally is cheaper and more convenient for development.

* `Instructions for OS X <http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/>`_
* `Instructions for Ubuntu 14.04 <https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04>`_
* `Installer for Windows <http://www.enterprisedb.com/products-services-training/pgdownload#windows>`_

Homebrew
========

You can use the Homebrew package manager to install PostgREST on Mac

.. code-block:: bash

  # Ensure brew is up to date
  brew update

  # Check for any problems with brew's setup
  brew doctor

  # Install the postgrest package
  brew install postgrest

This will automatically install PostgreSQL as a dependency. The process tends to take up to 15 minutes to install the package and its dependencies.

After installation completes, the tool is added to your $PATH and can be used from anywhere with:

.. code-block:: bash

  postgrest --help

