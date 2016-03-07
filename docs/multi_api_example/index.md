#Multi API example#

##How to set up PostgREST  with multiple apis and nginx in front on Debian##

This is written for Debian, and tested on Debian 8, Jessie

What we want to accomplish is a system with multiple (versions of) rest apis from a PostrgeSQL database.
We will do this with PostgREST. PostgREST handles multiple apis by letting you define them in different schemas preferable with views to your underlying data. Each api then needs its own process with its own portnumber. To translate addresses with port numbers to more common urls we put nginx webserver in front. That is also necessary for usage with ssl since all traffic then is going through port 465.

So, first install PostgREST as describe in the doc (link do installation doc here)

###Database Setup###
For this example, we have the following setup in the database:

Start by creating a database that we call **test_db**

```
/*Users and roles:
authenticator user, we use postgrest*/
CREATE ROLE postgrest NOINHERIT LOGIN;

/*For anonymous lgoin*/
CREATE ROLE anon;

/*and for usage through the rest API*/
CREATE ROLE rest_user;

/*Let postgrest switch to anon or rest_user*/
GRANT anon, rest_user TO postgrest;


/*Database schemas
Schema for data :*/
CREATE SCHEMA data_store;
/*A schema for our api verion 1*/
CREATE SCHEMA v1;

/*A schema for our api verion 2*/
CREATE SCHEMA v2;

/*Add some test data*/
CREATE TABLE data_store.test_data
(
	name TEXT,
	descr TEXT,
	length DOUBLE PRECISION,
	width DOUBLE PRECISION
);

INSERT INTO data_store.test_data(name, descr, length, width)
VALUES 
('Birch Plate', 'A beautiful plate of light birch for your kitchen', 2.523, 0.623),
('Oak Plate', 'A dark and hard plate for your kitchen ', 2.354, 0.618),
('Pine Plate', 'Good looking plate at good price ', 4.322, 0.675);

/*In our version 1 of the api we present the table “as is”*/
CREATE VIEW v1.plates AS
SELECT * FROM data_store.test_data;

/* In our api version 2 we have descided to give the measurements, not as in meter unit, but in millimeter.
We also want to give the area of the plate*/
CREATE VIEW v2.plates AS
SELECT name, descr, (length*1000)::INTEGER length_mm, (width*1000)::INTEGER width_mm, ROUND((length*width)::numeric,2) area_m2
FROM data_store.test_data;

/*Ok, let's set the right permissions. We use the anonymous user here to simplify*/
GRANT USAGE ON SCHEMA v1 TO anon; 
GRANT USAGE ON SCHEMA v2 TO anon; 

GRANT SELECT ON v1.plates TO anon;
GRANT SELECT ON v2.plates TO anon;
```
#####Summary of database#####
As you see we have no keys or constraints on those tables. 

We just have prepared two schema's for two different apis using the same data.

###About the unix user running PostgREST###
PostgreSQL have a feature that a local user o the server can connect directly to the database without logging in through unix-socket. That is very convenient since that mean you don't have to deal with passwords in your setup. It is also as secure as your server since it is not usable without access to the server.

To make it secure from any user on the system use the peer authentication method.

Then we have two things to do
1) Create a system user named postgres
2) Allow postgrest user to use peer authentication

1)
```
sudo adduser --home /dev/null --no-create-home --uid 65533 --disabled-password --gecos "" postgrest
```
2)
Open the file pg_hba.conf. On Debian with PG 9.5 it is found at /etc/postgresql/9.5/main/pg_hba.conf
If you have not edited that file you find a row quite close to the end of the file that look something like:
```
local   all             postgres                                peer
```
add user postgrest as user here so the line looks like:
```
local   all             postgres,postgrest                 peer
```

and restart the database server:
```
sudo service postgresql restart
```

####Test what you have got so far:####

Now we can start our iinstances manually to see if it works. As mentioned before the peer authentication method only allows us to connect to the db if we have the same system user name as database user. So we switch to user postgrest and starts.
Open two terminal windows and start one instance in each.

Terminal 1:
```
sudo su postgrest
/usr/local/bin/postgrest postgres://postgrest@:/test_db --port 3000 --pool 50 --max-rows 1000 --jwt-secret secret --schema v1 --anonymous anon
```
Terminal 2:
```
sudo su postgrest
/usr/local/bin/postgrest postgres://postgrest@:/test_db --port 3001 --pool 50 --max-rows 1000 --jwt-secret secret --schema v2 --anonymous anon
```
note the different schema names and port numbers

Now you can use a  web browser to see the output from the two apis using:
http://localhost:3000/plates
and
http://localhost:3001/plates

You can stop postgREST with ctrl-c

###Make services of this###
Now we want to put our two instances in services so they start on reboot andjust works in the background.

The setup used here is 2 config files and 2 init scripts, one for each api.

First we look at the config file. That is just a file holding the parameters that we give PostgREST:

First file we name /etc/default/postgrest_v1.conf
and the second is named:
/etc/default/postgrest_v2.conf

Examples of the init files: link to init files in example

Then we create 2 init scripts.

The only difference between the two is in the top parts where they point to different config files and have their api-name mentioned at 2 places.

Link to init scripts in example repo

Create directory for our log files

We put the log files in a postgrest directory, so create the directory and set user postgrest as owner:
sudo mkdir /var/log/postgrest
sudo chown postgrest /var/log/postgrest

Enable the services

sudo update-rc.d postgrest_v1 defaults
and
sudo update-rc.d postgrest_v2 defaults

Start the services

Now you should be able to start the services
sudo service postgrest_v1 start 
sudo service postgrest_v2 start 

and check their status
sudo service postgrest_v1 status

It has happened to me that it has seems like they are hanging when I start them.
Then stop them with ctrl-c and try restarting instead
sudo service postgrest_v2 restart 

Great, we now have 2 instances of PostgREST running as services


NGINX

As we mentioned earlier we are ow going to put a nginx web server in front of PostgREST. That is for at least 2 reasons:
1) If we want to use https nginx handles that part
2) We want more beautiful url in our api than including a port number

So, since we are using Debian we can just install nginx with a oneliner as usual:
sudo apt-get install nginx 

Then to do a minimal configuration we just add 2 rules to the default configuration.

So open the file
/etc/nginx/sites_enabled/default

In the server section there is a rule fore how to handle location /

just above that add:
        location /v1/ {
                proxy_pass http://127.0.0.1:3000/;
        }

        location /v2/ {
                proxy_pass http://127.0.0.1:3001/;

        }

close the file and reload nginx:
sudo service nginx reload

Now you should be able to access your plates “table” in the two apis from:
http://localhost/v1/plates
and
http://localhost/v2/plates


