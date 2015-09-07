export POSTGREST_VER=`grep ^version /app/postgrest.cabal | sed -En 's/.*\s+([0-9\.]+)/\1/p'`

curl -L http://sourceforge.net/projects/s3tools/files/s3cmd/1.5.0-alpha1/s3cmd-1.5.0-alpha1.tar.gz | tar zx

cp /app/dist/build/postgrest/postgrest postgrest-${POSTGREST_VER}
tar cJf postgrest-${POSTGREST_VER}.tar.xz postgrest-${POSTGREST_VER}

touch ~/.s3cfg

s3cmd-1.5.0-alpha1/s3cmd put --access_key=${S3_ACCESS_KEY} --secret_key=${S3_SECRET_KEY} -P -f postgrest-${POSTGREST_VER}.tar.xz $S3_BUCKET/postgrest-${POSTGREST_VER}.tar.xz
