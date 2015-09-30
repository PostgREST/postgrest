# TODO list to build debian "official" package

  It feels for free to modify, fix or take some task or all. 
  
## debian/control

* Fill description field
* Add Vcs-Browser
* Add Vcs-Git
* Add Uploaders field

## debian/copyright 

* Add more contributers 

## Dependencies packages

Some libraries dependencies aren't Debian package. Below is the list was built by [cabal-debian](https://wiki.debian.org/Haskell/CollabMaint/GettingStarted). These libraries are necessary to build Postgrest the right way. 

* libghc-base64-string-dev
* libghc-base64-string-prof
* libghc-bcrypt-dev
* libghc-bcrypt-prof
* libghc-hasql-dev
* libghc-hasql-prof
* libghc-hasql-backend-dev
* libghc-hasql-backend-prof
* libghc-hasql-postgres-dev
* libghc-hasql-postgres-prof
* libghc-string-conversions-dev
* libghc-string-conversions-prof
* libghc-wai-cors-dev
* libghc-wai-cors-prof
* libghc-wai-middleware-static-dev
* libghc-wai-middleware-static-prof
* libghc-hasql-dev
* libghc-hasql-backend-dev
* libghc-hasql-postgres-dev
* libghc-heredoc-dev
* libghc-hspec-wai-dev
* libghc-hspec-wai-json-dev
* libghc-http-media-dev
* libghc-packdeps-dev
* libghc-base64-string-doc
* libghc-bcrypt-doc
* libghc-hasql-doc
* libghc-hasql-backend-doc
* libghc-hasql-postgres-doc
* libghc-string-conversions-doc
* libghc-wai-cors-doc
* libghc-wai-middleware-static-doc

