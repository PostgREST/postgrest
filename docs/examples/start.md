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

Start the PostgREST server and point it at the new database.

```sh
postgrest -d demo1 -U postgres -a postgres --v1schema public
```

<div class="admonition note">
    <p class="admonition-title">Note about database users</p>

    <p>If you installed PostgreSQL with Homebrew on Mac then the
    database username may be your own login rather than
    <code>postgres</code>.</p>
</div>

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

Note that the server returns a multipart response with URL of each created resource.

```HTTP
Content-Type: application/json
Location: /festival?name=eq.Venice%20Film%20Festival


--postgrest_boundary
Content-Type: application/json
Location: /festival?name=eq.Cannes%20Film%20Festival
```

If you send a GET request to `/festival` it should return

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

At this point nominations are fully specified but it's not a convenient interface for a rest client. Let's make a view they can use. Paste this into `psql demo1`.

```sql
create or replace view nomination as
select comp.festival,  
       comp.name as competition,
       comp.year,
       film.title,
       film.director,
       film.rating
 from film_nomination as nom
 left join film on nom.film = film.id
 left join competition as comp on nom.competition = comp.id
 order by comp.year desc, comp.festival, competition;
```

Time to try it out. Let's get the contents of the new view, ordered by film rating

```
GET http://localhost:3000/nomination?order=rating.desc
```

If you find it more human readable, add an `Accept: text/csv` header.

### Releasing a New Version

Suppose we want this endpoint to cater to those moviegoers with attention deficit disorder. In today's busy world we don't have time to read an extra couple words or compare nuanced reviews. In API version two we will truncate the names and round the ratings!

Each version lives in a numbered schema, so let's make a schema for version two.

```sql
CREATE SCHEMA "2";
GRANT USAGE ON SCHEMA "2" TO PUBLIC;
ALTER DATABASE demo1 SET search_path = "2", "public";
```

To override the `films` endpoint create a view in the "2" schema with that name:

```sql
create or replace view "2".film as
select id, substring(f.title from 1 for 10) as title,
       year, director, round(f.rating) as rating, language
from "public".film as f;
```

We select the desired version as part of content negotiation. Try this get request:

```HTTP
GET http://localhost:3000/film
Accept: text/csv; version=2
```

Then try toggling the version string in the Accept header and watch the results change. Pretty good, now how about writing values? PostgreSQL's nice feature called auto-updatable views allows writes to pass through views. Sadly this view is not eligible because truncation and rounding cannot be uniquely reversed. If we attempt to post a new result it complains:

```json
{
  "hint": null,
  "details": "View columns that are not columns of their base relation are not updatable.",
  "code": "0A000",
  "message": "cannot insert into column \"title\" of view \"film\""
}
```

This is a case where we need explicit triggers

```sql
-- TODO - FIX THIS

-- CREATE OR REPLACE RULE insert_v2_films AS
--   ON INSERT TO "2".film
--   DO INSTEAD
--      INSERT INTO public.film (id, title, year, director, rating, language)
--      VALUES (NEW.id,     NEW.title,
--              NEW.year,   NEW.director,
--              NEW.rating, NEW.language)
--      RETURNING public.film.*;
```
