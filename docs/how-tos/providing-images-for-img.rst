.. _providing_img:

Providing images for ``<img>``
==============================

:author: `pkel <https://github.com/pkel>`_

In this how-to, you will learn how to create an endpoint for providing images to HTML :code:`<img>` tags without client side JavaScript. In fact, the presented technique is suitable for providing not only images, but arbitrary files.

We will start with a minimal example that highlights the general concept.
Afterwards we present a more detailed solution that fixes a few shortcomings of the first approach.

.. warning::

   Be careful when saving binaries in the database, having a separate storage service for these is preferable in most cases. See `Storing Binary files in the Database <https://wiki.postgresql.org/wiki/BinaryFilesInDB>`_.

Minimal Example
---------------

First, we need a public table for storing the files.

.. code-block:: postgres

   create table files(
     id   int primary key
   , blob bytea
   );

Let's assume this table contains an image of two cute kittens with id 42.
We can retrieve this image in binary format from our PostgREST API by requesting :code:`/files?select=blob&id=eq.42` with the :code:`Accept: application/octet-stream` header.
Unfortunately, putting the URL into the :code:`src` of an :code:`<img>` tag will not work.
That's because browsers do not send the required :code:`Accept: application/octet-stream` header.

Luckily we can specify the accepted media types in the :ref:`raw-media-types` configuration variable.
In this case, the :code:`Accept: image/webp` header is sent by many web browsers by default, so let's add it to the configuration variable, like this: :code:`raw-media-types="image/webp"`.
Now, the image will be displayed in the HTML page:

.. code-block:: html

   <img src="http://localhost:3000/files?select=blob&id=eq.42" alt="Cute Kittens"/>

Improved Version
----------------

The basic solution has some shortcomings:

1.  The response :code:`Content-Type` header is set to :code:`image/webp`.
    This might be a problem if you want to specify a different format for the file.
2.  Download requests (e.g. Right Click -> Save Image As) to :code:`/files?select=blob&id=eq.42` will propose :code:`files` as filename.
    This might confuse users.
3.  Requests to the binary endpoint are not cached.
    This will cause unnecessary load on the database.

The following improved version addresses these problems.
First, in addition to the minimal example, we need to store the media types and names of our files in the database.

.. code-block:: postgres

   alter table files
     add column type text,
     add column name text;

Next, we set up an RPC endpoint that sets the content type and filename.
We use this opportunity to configure some basic, client-side caching.
For production, you probably want to configure additional caches, e.g. on the :ref:`reverse proxy <admin>`.

.. code-block:: postgres

   create function file(id int) returns bytea as
   $$
     declare headers text;
     declare blob bytea;
     begin
       select format(
         '[{"Content-Type": "%s"},'
          '{"Content-Disposition": "inline; filename=\"%s\""},'
          '{"Cache-Control": "max-age=259200"}]'
         , files.type, files.name)
       from files where files.id = file.id into headers;
       perform set_config('response.headers', headers, true);
       select files.blob from files where files.id = file.id into blob;
       if found
       then return(blob);
       else raise sqlstate 'PT404' using
         message = 'NOT FOUND',
         detail = 'File not found',
         hint = format('%s seems to be an invalid file id', file.id);
       end if;
     end
   $$ language plpgsql;

With this, we can obtain the cat image from :code:`/rpc/file?id=42`. Thus, the resulting HTML will be:

.. code-block:: html

   <img src="http://localhost:3000/rpc/file?id=42" alt="Cute Kittens"/>
