.. _ghc:

GHC
===

PostgREST uses the `GHC (Glasgow Haskell Compiler) runtime system <https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html>`_ for memory management and thread scheduling. You can tune its behavior through the ``GHCRTS`` environment variable.

Garbage Collection
------------------

By default ``-A16m`` is used to set the garbage collector allocation area size. This has proven to be a sane default but for machines with larger core counts (32 or more), you can set ``GHCRTS='-A64m'`` to increase throughput:

.. code-block:: bash

  GHCRTS="-A64m" ./postgrest postgrest.conf

You can set :ref:`log-level` to ``debug`` to confirm these settings were applied, this will log all the RTS flags at startup.

.. code-block::

  05/Oct/2026:22:28:01 -0500: RTSFlags {gcFlags = GCFlags {... minAllocAreaSize = 16384 ...

See more details on https://github.com/PostgREST/postgrest/issues/5222.
