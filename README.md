## Developing locally

The javascript inclusion requires the site to be hosted as `http://`.
The easiest way I know to host a bunch of static files on localhost is
this:

```sh
python -m SimpleHTTPServer
```

To get Grunt to properly compile, you'll need to run

```
npm install
```

then

```
grunt
```

for it to do its thing. When working locally, run

```
grunt watch
```

You can [download the LiveReload browser extensions](http://feedback.livereload.com/knowledgebase/articles/86242-how-do-i-install-and-use-the-browser-extensions) to have styles and content automatically refresh.

## Deployment to production

Push changes to the `gh-pages` branch. Done.
