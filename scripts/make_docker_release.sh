RELEASE_DIR=${RELEASE_DIR:-./.release}

mkdir -p $RELEASE_DIR

stack install --allow-different-user --local-bin-path $RELEASE_DIR
if [[ -e ${RELEASE_DIR}/postgrest ]]; then
    echo 'Postgrest binary exists.. \n';
    docker-compose build release;
else
    echo "Postgrest binary does NOT exist\n";
    exit 1;
fi
