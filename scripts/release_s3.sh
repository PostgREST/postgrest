export DBAPI_VER=`grep ^version /app/dbapi.cabal | sed -En 's/.*\s+([0-9\.]+)/\1/p'`

curl -L http://softlayer-ams.dl.sourceforge.net/project/s3tools/s3cmd/1.5.0-alpha1/s3cmd-1.5.0-alpha1.tar.gz | tar zx

cp /app/dist/build/dbapi/dbapi dbapi-${DBAPI_VER}
tar cJf dbapi-${DBAPI_VER}.tar.xz dbapi-${DBAPI_VER}

touch ~/.s3cfg

s3cmd-1.5.0-alpha1/s3cmd put --access_key=${S3_ACCESS_KEY} --secret_key=${S3_SECRET_KEY} -P -f dbapi-${DBAPI_VER}.tar.xz $S3_BUCKET/dbapi-${DBAPI_VER}.tar.xz
