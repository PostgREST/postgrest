## Deployment

### Heroku

#### Account setup on Heroku
Setting up a simple Heroku “Free Account” is easy and should take no more than a couple of minutes. For this guide I will be using a fictitious email account “postgrest@kismail.ru” to set up the account on Heroku. You can use whatever email account you desire.
NOTE: During signup you will be asked to “Pick your primary development language”. It is safe here to choose any language or simply “I use another language”.

1. Heroku signup input
![heroku signup input](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/1_heroku_signup_input.png)<br>
2. Heroku signup waiting
![heroku signup waiting](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/2_heroku_signup_waiting.png)<br>
3. Heroku signup confirmation
![heroku signup confirmation](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/3_heroku_signup_confirmation.png)<br>
4. Heroku signup password
![heroku signup password](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/4_heroku_signup_password.png)<br>
5. Heroku signup success
![heroku signup success](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/5_heroku_signup_success.png)<br>

After initial signup, you will receive a confirmation email. You have to open the link provided in the email to activate your Heroku Free Account.

#### New “App” setup in Heroku
Creating an “App” can be seen as creating an environment, complete with hardware resources, to run any code you wish. It is here that the PostgreSQL database and the PostgREST instance will reside.

Select “New App” and then select a new name for the “App”. It can be anything as long as it is available. The “Runtime Selection” region can be anywhere, it only specifies where the app data is stored.

1. Heroku initial empty page
![heroku initial empty page](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/6_heroku_initial_empty_page.png)<br>
2. Heroku create new app
![heroku create new app](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/7_heroku_create_new_app.png)<br>
3. Heroku app creation success
![heroku app creation success](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/8_heroku_app_created_success.png)<br>

#### Setup of PostgreSQL add-on in newly setup “App”
At this point you should have your new “App” environment setup and running. 
It is now time to add a PostgreSQL database to your “App”.
In your “App” screen, you will have several tabs with names such as “Overview”, “Resources”, “Deploy”, etc. 

1. Choose “Resources”. This will show you the resources currently running in your “App”, which should be empty as you have yet to deploy any resources.
2. On the “Resources” page, under the header “Add-ons” you will find a search bar. Type in “Postgres”, which will show you “Heroku Postgres”. Click on the name to show a deployment dialog.
![heroku find postgresql db](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/9_heroku_find_postgresql_db.png)<br>
![heroku install postgresql db](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/10_heroku_install_postgresql_db.png)<br>
3. After deployment, you will see a line in your “Add-ons” named “Heroku Postgres”. You have now added a PostgreSQL database to your “App”. Now to setup the database.
NOTE: It might take a few minutes for Heroku to initialize the db, so please wait 1-2 minutes before moving on to step 4.
![heroku installed postgresql db](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/11_heroku_installed_postgresql_db.png)<br>
4. Click on the newly added “Heroku Postgres” in your “Add-ons” and you will be taken to the database “Overview” page. Here you will find all the information necessary to find and connect to your PostgreSQL DB. This information will be very important when setting up PostgREST.
![heroku db panel](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/12_heroku_db_panel.png)<br>
![heroku db overview](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/13_heroku_db_overview.png)<br>
![heroku db stats](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/14_heroku_db_stats.png)<br>
![heroku db success](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/15_heroku_app_with_db.png)<br>
5. At this point, you have full access to the PostgreSQL database and can create tables, schemas, functions, etc. 
NOTE: The PostgREST setup will only work against a single Schema. This means that tables outside of the selected Schema will not be directly accessible via API calls. 

#### Testing of newly setup PostgreSQL
Before we move on to deploying PostgREST, we could perform a quick test to see that our DB is up and running. This can be done a many different ways, but I will provide links for GUI with pgAdmin:<br> http://agileforce.co.uk/heroku-workshop/heroku-postgres/pgadmin.html .<br>
It is also possible to connect with psql for simple testing.
NOTE: When using pgAdmin to connect to the DB, you will see, but not have access to, many other DB’s. Don’t be alarmed. Find your DB by the “Database“ name provided in the Overview.

#### Setup of PostgREST deployment
Now it is time to deploy PostgREST. 
The PostgREST deployment on Heroku is not an “Add-on”, it is an “Element”. It can be found by opening the menu at the top right of the Heroku screen and selecting “Elements”. Search for “PostgREST” and find it under “Buttons”. Opening the “PostgREST” page, a “Deploy to Heroku” button will be present on the page. This button will lead to the initial setup and configuration page for the PostgREST deployment on your “App”.

1. Heroku find postgREST 1
![heroku find postgrest 1](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/16_heroku_find_postgrest_1.png)<br>
2. Heroku find postgREST 2
![heroku find postgrest 2](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/17_heroku_find_postgrest_2.png)<br>
3. Heroku postgREST page
![heroku postgrest page](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/18_heroku_postgrest_page.png)<br>
4. Heroku postgREST install
![heroku postgrest install](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/19_heroku_postgrest_installation.png)<br>

Explanations for each field in the PostgREST setup page:

- **App Name** (optional) : The name chosen for the PostgREST deployment. 
NOTE: The name you choose will be the beginning of your specific API url. So if you choose “bugbunny”, your API url will be “bugbunny.herokuapp.com”.
- **Runtime Selection** : Simply select the region the deployment (basically data) will be stored.
- **BUILDPACK_URL** : Do not change this. This points to the pack that will be used for setup, which cannot be changed if you want a functioning PostgREST.
- **POSTGREST_VER** : Do not change this. Version of PostgREST used for installation.

POSTGREST configs (Information from the PostgreSQL Overview page):
- **DB_NAME** : Here you must provide the name of the database you setup earlier, which can be found on the “Overview” page of the PostgreSQL add-on. It should be something like “d41vontrqmq2oo”.
- **AUTH_ROLE** : This is the role of an authenticated user. 
All authenticated calls to the API will use this user role when attempting to perform actions against the PostgreSQL database. User authentication is done using the procedure described on the Security page in the PostgREST guide, link provided at the bottom. For the purpose of this demo, you can simply input the data in the “User” field of the Overview page of the PostgreSQL add-on. It should be something like “nhvnhzryphzmcp”.
- **AUTH_PASS** : Simply the password for the authenticated user, also found on the Overview page of the PostgreSQL add-on. It should be something like “5YDLXynT2ZWqUA6c2c_D12TofA”.
- **ANONYMOUS_ROLE** : This is the role for any unauthenticated call to the API. 
In a regular PostgreSQL database this role could be for calls that for example don’t alter data, but simply retrieve data. Unfortunately in Heroku Hobby-dev account, it is not possible to add any roles to your PostgreSQL database, which means you will have to use the only role provided when PostgreSQL is setup, the admin role. So input the same data provided in the “AUTH_ROLE” field.
Please keep in mind that this admin role has full access and can do anything, including deleting from the db.
- **DB_HOST** : This is the URL to the database, which should be copied directly from the Overview page of the PostgreSQL add-on.
- **DB_PORT** : The port to used when connecting to the database. Usually not necessary to change unless some specific manual changes have been made to the DB connection.
- **DB_POOL** : As the description states, “Maximum number of connections in database pool”. This parameter is not necessary to change.
- **JWT_SECRET** : Information regarding this parameter can be found in the Security link provided. For testing purposes, there is no need to change this parameter, but it is important to read and fully understand how JWT works if and when moving on from testing only.
- **SCHEMA** : This parameter is very important when it comes to being able to access the information on the PostgreSQL DB via PostgREST, as it indicates what Schema on the DB should be made available through the API calls. Only a single Schema can be selected and the tables and views in that Schema will be accessible through the API, so please make sure that the correct Schema name is input here.

When all the above fields are filled in, the PostgREST configuration should be ready for deployment. Pressing the “Deploy for free” button at the bottom of the screen will initiate the deployment process, which should take no more than a 1-2 minutes to finish.
![heroku postgrest installing](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/20_heroku_postgrest_installing.png)<br>
![heroku postgrest installed](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/21_heroku_postgrest_installed.png)<br>

#### Testing the PostgREST deployment
At this point, you should have the PostgreSQL database and the PostgREST Heroku element up and running. Time to test the setup. 
The easiest way to see if things are up and running is to go to your “Personal Apps” in your Heroku dashboard, select the name you selected when deploying PostgREST (like “bugbunny”) and then pressing the “Open app” button at the top, right corner of the screen. What should open is a new webpage, with a JSON representation of the tables in your PostgreSQL database. (https://bugbunny.herokuapp.com/)
![heroku postgrest test 1](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/22_heroku_postgrest_test_1.png)<br>
An additional test that can be performed is to add the name of any table inside you database Schema to the end of the PostgREST Heroku app URL, such as https://bugbunny.herokuapp.com/people, where “people” is the name of the Table in the database Schema. This will retrieve, in JSON form, all the rows in the “People” table.
Next steps
![heroku postgrest test 1](https://github.com/Mithrandir21/postgrest/blob/master/docs/img/heroku_guide/23_heroku_postgrest_test_2.png)<br>

<br><br>
Heroku LINKS:<br>
PostgreSQL - https://devcenter.heroku.com/articles/heroku-postgres-plans <br>
PostgREST Security - http://postgrest.com/admin/security/ <br>
Heroku PostgreSQL - https://elements.heroku.com/addons/heroku-postgresql <br>
Heroku Postgrest - https://elements.heroku.com/buttons/begriffs/postgrest <br>
<br><br>

#### Getting Started

#### Using Amazon RDS

### Debian
