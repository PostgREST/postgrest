RELEASE_DIR=${RELEASE_DIR:-./.release}

if [[ -e ${RELEASE_DIR}/postgrest ]]; then
    echo 'Postgrest binary exists.. \n';
    docker-compose build release;
else
    echo "Postgrest binary does NOT exist\n";
    exit 1;
fi
