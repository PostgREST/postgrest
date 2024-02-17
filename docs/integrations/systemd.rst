systemd
=======

For Linux distributions that use **systemd** (Ubuntu, Debian, Archlinux) you can create a daemon in the following way.

First, create postgrest configuration in ``/etc/postgrest/config``

.. code-block:: ini

  db-uri = "postgres://<your_user>:<your_password>@localhost:5432/<your_db>"
  db-schemas = "<your_exposed_schema>"
  db-anon-role = "<your_anon_role>"
  jwt-secret = "<your_secret>"

Then create the systemd service file in ``/etc/systemd/system/postgrest.service``

.. code-block:: ini

  [Unit]
  Description=REST API for any PostgreSQL database
  After=postgresql.service

  [Service]
  ExecStart=/bin/postgrest /etc/postgrest/config
  ExecReload=/bin/kill -SIGUSR1 $MAINPID

  [Install]
  WantedBy=multi-user.target

After that, you can enable the service at boot time and start it with:

.. code-block:: bash

  systemctl enable postgrest
  systemctl start postgrest

  ## For reloading the service
  ## systemctl restart postgrest

.. _file_descriptors:

File Descriptors
----------------

File descriptors are kernel resources that are used by HTTP connections (among others). File descriptors are limited per process. The kernel default limit is 1024, which is increased in some Linux distributions.
When under heavy traffic, PostgREST can reach this limit and start showing ``No file descriptors available`` errors. To clear these errors, you can increase the process' file descriptor limit.

.. code-block:: ini

  [Service]
  LimitNOFILE=10000
