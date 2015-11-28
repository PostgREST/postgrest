## Getting Started

### Your First (simple) API

Let's start with the simplest thing possible. We will expose some tables directly for reading and writing by anyone.

Start by making a database

```sh
createdb demo1
```

We'll set it up with a film example (courtesy of [Jonathan Harrington](http://blog.jonharrington.org/postgrest-introduction/)). Copy the following into your clipboard:

```sql
BEGIN;

CREATE TABLE director
(
  name text NOT NULL PRIMARY KEY
);

CREATE TABLE film
(
  id serial PRIMARY KEY,
  title text NOT NULL,
  year date NOT NULL,
  director text,
  rating real NOT NULL DEFAULT 0,
  language text NOT NULL,
  CONSTRAINT film_director_fkey FOREIGN KEY (director)
      REFERENCES director (name) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE festival
(
  name text NOT NULL PRIMARY KEY
);

CREATE TABLE competition
(
  id serial PRIMARY KEY,
  name text NOT NULL,
  festival text NOT NULL,
  year date NOT NULL,

  CONSTRAINT comp_festival_fkey FOREIGN KEY (festival)
      REFERENCES festival (name) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE TABLE film_nomination
(
  id serial PRIMARY KEY,
  competition integer NOT NULL,
  film integer NOT NULL,
  won boolean NOT NULL DEFAULT true,

  CONSTRAINT nomination_competition_fkey FOREIGN KEY (competition)
     REFERENCES competition (id) MATCH SIMPLE
     ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT nomination_film_fkey FOREIGN KEY (film)
     REFERENCES film (id) MATCH SIMPLE
     ON UPDATE CASCADE ON DELETE CASCADE
);

COMMIT;
```

Apply it to your new database by running

```sh
# On OS X
pbpaste | psql demo1

# Or Linux
# xclip -selection clipboard -o | psql demo1
```

Start the PostgREST server and point it at the new database. (See the [installation instructions](/install/server/).)

```sh
postgrest postgres://postgres:@localhost:5432/demo1 -a postgres --schema public
```

<div class="admonition note">
    <p class="admonition-title">Note about database users</p>

    <p>If you installed PostgreSQL with Homebrew on Mac then the
    database username may be your own login rather than
    <code>postgres</code>.</p>
</div>

### Populating Data

Let's use PostgREST to populate the database. Install a REST client such as [Postman](https://chrome.google.com/webstore/detail/postman/fhbjgbiflinjbdggehcddcbncdddomop?hl=en). Now let's insert some data as a bulk post in CSV format:

```HTTP
POST http://localhost:3000/festival
Content-Type: text/csv

name
Venice Film Festival
Cannes Film Festival
```

In Postman it will look like this

![Festival bulk insert in postman](/img/post-festivals.png)

Notice that the post type is `raw` and that `Content-Type: text/csv` set in the Headers tab.

The server returns HTTP 201 Created. Because we inserted more than one item at once there is no `Location` header in the response. However sometimes you want to learn more about items which you just inserted. To have the server include the full restuls include the header `Prefer: return=representation`.

At this point if you send a GET request to `/festival` it should return

```json
[
  {
    "name": "Venice Film Festival"
  },
  {
    "name": "Cannes Film Festival"
  }
]
```

Now that you've seen how to do a bulk insert, let's do some more and fully populate the database.

Post the following to `/competition`:

```csv
name,festival,year
Golden Lion,Venice Film Festival,2014-01-01
Palme d'Or,Cannes Film Festival,2014-01-01
```

Now `/director`:

```csv
name
Bertrand Bonello
Atom Egoyan
David Gordon Green
Andrey Konchalovskiy
Mario Martone
Mike Leigh
Roy Andersson
Saverio Costanzo
Alix Delaporte
Jean-Pierre Dardenne
Xiaoshuai Wang
Kaan Müjdeci
Tommy Lee Jones
Nuri Bilge Ceylan
Michel Hazanavicius
Xavier Dolan
Ramin Bahrani
Alice Rohrwacher
Andrew Niccol
Rakhshan Bani-Etemad
David Oelhoffen
Bennett Miller
David Cronenberg
Shin'ya Tsukamoto
Joshua Oppenheimer
Olivier Assayas
Jean-Luc Godard
Alejandro González Iñárritu
Benoît Jacquot
Fatih Akin
Francesco Munzi
Ken Loach
Abel Ferrara
Xavier Beauvois
Naomi Kawase
```

And `/film`:

```csv
title,year,director,rating,language
Chuang ru zhe,2014-01-01,Xiaoshuai Wang,6.19999981,english
The Look of Silence,2014-01-01,Joshua Oppenheimer,8.30000019,Indonesian
Fires on the Plain,2014-01-01,Shin'ya Tsukamoto,5.80000019,Japanese
Far from Men,2014-01-01,David Oelhoffen,7.5,english
Good Kill,2014-01-01,Andrew Niccol,6.0999999,english
Leopardi,2014-01-01,Mario Martone,6.9000001,english
Sivas,2014-01-01,Kaan Müjdeci,7.69999981,english
Black Souls,2014-01-01,Francesco Munzi,7.0999999,english
Three Hearts,2014-01-01,Benoît Jacquot,5.80000019,French
Pasolini,2014-01-01,Abel Ferrara,5.80000019,english
Le dernier coup de marteau,2014-01-01,Alix Delaporte,6.5,english
Manglehorn,2014-01-01,David Gordon Green,7.0999999,english
Hungry Hearts,2014-01-01,Saverio Costanzo,6.4000001,English
Belye nochi pochtalona Alekseya Tryapitsyna,2014-01-01,Andrey Konchalovskiy,6.9000001,Russian
99 Homes,2014-01-01,Ramin Bahrani,7.30000019,english
The Cut,2014-01-01,Fatih Akin,6,Armenian
Birdman: Or (The Unexpected Virtue of Ignorance),2014-01-01,Alejandro González Iñárritu,8,English
La rançon de la gloire,2014-01-01,Xavier Beauvois,5.69999981,French
A Pigeon Sat on a Branch Reflecting on Existence,2014-01-01,Roy Andersson,7.19999981,english
Tales,2014-01-01,Rakhshan Bani-Etemad,6.80000019,english
The Wonders,2014-01-01,Alice Rohrwacher,6.80000019,Italian
Foxcatcher,2014-01-01,Bennett Miller,7.19999981,English
Mr. Turner,2014-01-01,Mike Leigh,7,English
Jimmy's Hall,2014-01-01,Ken Loach,6.69999981,English
The Homesman,2014-01-01,Tommy Lee Jones,6.5999999,English
The Captive,2014-01-01,Atom Egoyan,5.9000001,english
Goodbye to Language,2014-01-01,Jean-Luc Godard,6.19999981,French
The Search,2014-01-01,Michel Hazanavicius,6.9000001,French
Still the Water,2014-01-01,Naomi Kawase,6.9000001,Japanese
Mommy,2014-01-01,Xavier Dolan,8.30000019,French
"Two Days, One Night",2014-01-01,Jean-Pierre Dardenne,7.4000001,French
Maps to the Stars,2014-01-01,David Cronenberg,6.4000001,English
Saint Laurent,2014-01-01,Bertrand Bonello,6.5,French
Clouds of Sils Maria,2014-01-01,Olivier Assayas,6.9000001,english
Winter Sleep,2014-01-01,Nuri Bilge Ceylan,8.5,Turkish
```

Finally `/film_nomination`:

```csv
competition,film,won
1,1,f
1,2,f
1,3,f
1,4,f
1,5,f
1,6,f
1,7,f
1,8,f
1,9,f
1,10,f
1,11,f
1,12,f
1,13,f
1,14,f
1,15,f
1,16,f
1,17,f
1,18,f
1,19,f
1,20,f
2,21,f
2,22,f
2,23,f
2,24,f
2,25,f
2,26,f
2,27,f
2,28,f
2,29,f
2,30,f
2,31,f
2,32,f
2,33,f
2,34,f
2,35,f
```

### Getting and Embedding Data

First let's review which films are stored in the database:
```http
GET http://localhost:3000/film
```
It gives us back a list of JSON objects. What if we care only about the film titles? Use `select` to shape the output:

```http
GET http://localhost:3000/film?select=title
```
```json
[
  {
    "title": "Chuang ru zhe"
  },
  {
    "title": "The Look of Silence"
  },
  {
    "title": "Fires on the Plain"
  },
  ...
]
```

Here is where it gets cool. PostgREST can embed objects in its response through foreign key relationships. Earlier we created a join table called `film_nomination`. It joins films and competitions. We can ask the server about the structure of this table:

```
OPTIONS http://localhost:3000/film_nomination
```

```json
{
  "pkey": [
    "id"
  ],
  "columns": [
    {
      "references": null,
      "default": "nextval('film_nomination_id_seq'::regclass)",
      "precision": 32,
      "updatable": true,
      "schema": "public",
      "name": "id",
      "type": "integer",
      "maxLen": null,
      "enum": [],
      "nullable": false,
      "position": 1
    },
    {
      "references": {
        "schema": "public",
        "column": "id",
        "table": "competition"
      },
      "default": null,
      "precision": 32,
      "updatable": true,
      "schema": "public",
      "name": "competition",
      "type": "integer",
      "maxLen": null,
      "enum": [],
      "nullable": false,
      "position": 2
    },
    {
      "references": {
        "schema": "public",
        "column": "id",
        "table": "film"
      },
      "default": null,
      "precision": 32,
      "updatable": true,
      "schema": "public",
      "name": "film",
      "type": "integer",
      "maxLen": null,
      "enum": [],
      "nullable": false,
      "position": 3
    },
    {
      "references": null,
      "default": "true",
      "precision": null,
      "updatable": true,
      "schema": "public",
      "name": "won",
      "type": "boolean",
      "maxLen": null,
      "enum": [],
      "nullable": false,
      "position": 4
    }
  ]
}
```

From this you can see that the columns `film` and `competition` reference their eponymous tables. Let's ask the server for each film along with names of the competitions it entered. You don't have to do any custom coding. Send this query:

```http
GET http://localhost:3000/film?select=title,competition{name}
```

```json
[
  {
    "title": "Chuang ru zhe",
    "competition": [
      {
        "name": "Golden Lion"
      }
    ]
  },
  {
    "title": "The Look of Silence",
    "competition": [
      {
        "name": "Golden Lion"
      }
    ]
  },
  ...
]
```

The relation flows both ways. Here is how to get the name of each competition's name and the movies shown at it.

```http
GET http://localhost:3000/competition?select=name,film{title}
```

```json
[
  {
    "name": "Golden Lion",
    "film": [
      {
        "title": "Chuang ru zhe"
      },
      {
        "title": "The Look of Silence"
      },
      ...
    ]
  },
  {
    "name": "Palme d'Or",
    "film": [
      {
        "title": "The Wonders"
      },
      {
        "title": "Foxcatcher"
      },
      ...
    ]
  }
]
```

Why not learn about the directors too? There is a many-to-one relation directly between films and directors. We can alter our previous query to include directors in its results.


```http
GET http://localhost:3000/competition?select=name,film{title,director{*}}
```

```json
[
  {
    "name": "Golden Lion",
    "film": [
      {
        "title": "Manglehorn",
        "director": {
          "name": "David Gordon Green"
        }
      },
      {
        "title": "Belye nochi pochtalona Alekseya Tryapitsyna",
        "director": {
          "name": "Andrey Konchalovskiy"
        }
      },
      ...
    ]
  },
  ...
]
```

### Singular Responses

How do we ask for a single film, for instance the second one we inserted?

```http
GET http://localhost:3000/film?id=eq.2
```
It returns
```json
[
  {
    "id": 2,
    "title": "The Look of Silence",
    "year": "2014-01-01",
    "director": "Joshua Oppenheimer",
    "rating": 8.3,
    "language": "Indonesian"
  }
]
```

Like any query, it gives us a result *set*, in this case an array with one element. However you and I know that `id` is a primary key, it will never return more than one result. We might want it returned as a JSON object, not an array. To express this preference include the header `Prefer: plurality=singular`. It will respond with


```json
{
  "id": 2,
  "title": "The Look of Silence",
  "year": "2014-01-01",
  "director": "Joshua Oppenheimer",
  "rating": 8.3,
  "language": "Indonesian"
}
```

<div class="admonition note">
    <p class="admonition-title">Why this approach to singular responses?</p>

    <p>
    PostgREST knows which columns comprise a primary key for a
    table, so why not automatically choose plurality=singular when
    these column filters are present? The fact is it could come as a
    shock to a client that by adding one more filter condition it can
    change the entire response format.
    </p>
    <p>
    Then why not expose another kind of route such as /film/2 to indicate
    one particular film? Because this does not accommodate compound keys.
    The convention complects a plurality preference with table key
    assumptions. We should separate concerns.
    </p>
    <p>
    It turns out you can still have routes like /film/2.  Use a
    proxy such as Nginx. It can rewrite routes such as /films/2
    into /films?id=eq.2 and add the Prefer header to make the results
    singular.
    </p>
</div>

### Conclusion

This tutorial showed how to create a database with a basic schema, run PostgREST, and interact with the API. The next tutorial will show how to enable security for a multi-tenant blogging API.
