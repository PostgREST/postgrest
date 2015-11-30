<style>
.videoWrapper {
  position: relative;
  padding-bottom: 56.25%; /* 16:9 */
  padding-top: 25px;
  height: 0;
}
.videoWrapper iframe {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
}
</style>
![PostgREST logo](img/logo.png)

## Introduction

PostgREST is a standalone web server that turns your database directly into a RESTful API. The structural constraints and permissions in the database determine the API endpoints and operations.

This guide explains how to install the software and provides practical examples of its use. You'll learn how to build a fast, versioned, secure API and how to deploy it to production.

The project has a friendly and growing community. Here are some ways to get help or get involved:

* The project [chat room](https://gitter.im/begriffs/postgrest)
* Report or search [issues](https://github.com/begriffs/postgrest/issues)

### Motivation

Using PostgREST is an alternative to manual CRUD programming. Custom API servers suffer problems. Writing business logic often duplicates, ignores or hobbles database structure. Object-relational mapping is a leaky abstraction leading to slow imperative code. The PostgREST philosophy establishes a single declarative source of truth: the data itself.

#### Declarative Programming

It's easier to ask Postgres to join data for you and let its query planner figure out the details than to loop through rows yourself. It's easier to assign permissions to db objects than to add guards in controllers. (This is especially true for cascading permissions in data dependencies.) It's easier set constraints than to litter code with sanity checks.

#### Leakproof Abstraction

There is no ORM involved. Creating new views happens in SQL with known performance implications. A database administrator can now create an API from scratch with no custom programming. 

#### Embracing the Relational Model

In 1970 E. F. Codd criticized the then-dominant hierarchical model of databases in his article <a href="https://www.seas.upenn.edu/~zives/03f/cis550/codd.pdf">A Relational Model of Data for Large Shared Data Banks</a>. Reading the article reveals a striking similarity between hierarchical databases and nested http routes. With PostgREST we attempt to use flexible filtering and embedding rather than nested routes.

#### One Thing Well

PostgREST has a focused scope. It works well with other tools like Nginx. This forces you to cleanly separate the data-centric CRUD operations from other concerns. Use a collection of sharp tools rather than building a big ball of mud.

#### Shared Improvements

As with any open source project, we all gain from features and fixes in the tool. It's more beneficial than improvements locked inextricably within custom codebases.

### Intro Video

Some things have changed since this video was created but the basics are the same. Learn the big vision behind automating APIs.

<div class="videoWrapper">
<iframe src="https://player.vimeo.com/video/115668217" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>
</div>

### Myths

#### You have to make tons of stored procs and triggers

Modern PostgreSQL features like auto-updatable views and computed columns make this mostly unnecessary. Triggers do play a part, but generally not for irksome boilerplate. When they are required triggers are preferable to ad-hoc app code anyway, since the former work reliably for any codepath.

#### Exposing the database destroys encapsulation

PostgREST does versioning through database schemas. This allows you to expose tables and views without making the app brittle. Underlying tables can be superseded and hidden behind public facing views. The chapter about versioning shows how to do this.

### Conventions

This guide contains highlighted notes and tangential information interspersed with the text.

<div class="admonition note">
    <p class="admonition-title">Design Consideration</p>

    <p>Contains history which informed the current design. Sometimes it discusses unavoidable tradeoffs or a point of theory.</p>
</div>

<div class="admonition warning">
    <p class="admonition-title">Invitation to Contribute</p>

    <p>Points out things we know we want to add or improve. They might give you ideas for ways to contribute to the project.</p>
</div>

<div class="admonition danger">
    <p class="admonition-title">Deprecation Warning</p>

    <p>Alerts you to features which will be removed in the next major (breaking) release.</p>
</div>
