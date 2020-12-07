.. _providing_img:

Providing images for ``<img>``
==============================

:author: `pkel <https://github.com/pkel>`_

In this how-to, you will learn how to create an endpoint for providing images to HTML :code:`<img>` tags without client side JavaScript.
The resulting HTML might look like this:

.. code-block:: html

   <img src="http://host/files/42/cats.jpeg" alt="Cute Kittens"/>

In fact, the presented technique is suitable for providing not only images, but arbitrary files.

We will start with a minimal example that highlights the general concept.
Afterwards we present are more detailed solution that fixes a few shortcomings of the first approach.

Minimal Example
---------------

PostgREST returns binary data on requests that set the :code:`Accept: application/octet-stream` header.
The general idea is to configure the reverse proxy in front of the API to set this header for all requests to :code:`/files/`.
We will show how to achieve this using Nginx.

First, we need a public table for storing the files.

.. code-block:: postgres

   create table files(
     id   int primary key
   , blob bytea
   );

Let's assume this table contains an image of two cute kittens with id 42.
We can retrieve this image in binary format from our PostgREST API by requesting :code:`/files?select=blob&id=eq.42` with the :code:`Accept: application/octet-stream` header.
Unfortunately, putting the URL into the :code:`src` of an :code:`<img>` tag will not work.
That's because browsers do not send the required header.

Luckily, we can configure our :ref:`Nginx reverse proxy <admin>` to fix this problem for us.
We assume that PostgREST is running on port 3000.
We provide a new location :code:`/files/` that redirects requests to our endpoint with the :code:`Accept` header set to :code:`application/octet-stream`.

.. code-block:: nginx

   server {
     # rest of reverse proxy and web server configuration
     ...

     location /files/ {
       # /files/<id>/* ---> /files?select=blob&id=eq.<id>
       rewrite /files/([^/]+).*  /files?select=blob&id=eq.$1  break;
       # if id is missing
       return 404;
       # request binary output
       proxy_set_header Accept application/octet-stream;
       # usual proxy setup
       proxy_hide_header Content-Location;
       add_header Content-Location /api/$upstream_http_content_location;
       proxy_set_header  Connection "";
       proxy_http_version 1.1;
       proxy_pass http://localhost:3000/;
     }

With this setup, we can request the cat image at :code:`localhost/files/42/cats.jpeg` without setting any headers.
In fact, you can replace :code:`cats.jpeg` with any other filename or simply omit it.
Putting the URL into the :code:`src` of an :code:`<img>` tag should now work as expected.

Improved Version
----------------

The basic solution has some shortcomings:

1.  The response :code:`Content-Type` header is set to :code:`application/octet-stream`.
    This might confuse clients and users.
2.  Download requests (e.g. Right Click -> Save Image As) to :code:`files/42` will propose :code:`42` as filename.
    This might confuse users.
3.  Requests to the binary endpoint are not cached.
    This will cause unnecessary load on the database.

The following improved version addresses these problems.
First, we store the media types and names of our files in the database.

.. code-block:: postgres

   create table files(
     id   int primary key
   , type text
   , name text
   , blob bytea
   );

Next, we set up an RPC endpoint that sets the content type and filename.
We use this opportunity to configure some basic, client-side caching.
For production, you probably want to configure additional caches, e.g. on the reverse proxy.

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

With this, we can obtain the cat image from :code:`/rpc/file?id=42`.
Consequently, we have to replace our previous rewrite rule in the Nginx recipe with the following.

.. code-block:: nginx

   rewrite /files/([^/]+).*  /rpc/file?id=$1  break;
