FROM debian:jessie

# PostgREST reads /etc/postgrest.conf so map the configuration
# file in when you run this container
CMD ["postgrest"]

EXPOSE 3000

RUN apt-get update && \
    apt-get install -y libpq5 && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV POSTGREST_VERSION 0.4.0.0
