FROM debian:jessie

# PostgREST reads /etc/postgrest.conf so map the configuration
# file in when you run this container
ENTRYPOINT ["/sbin/pid1", "postgrest"]
CMD ["--help"]

EXPOSE 3000

ADD https://github.com/fpco/pid1/releases/download/pid1%2F0.1.0.1/pid1-0.1.0.1-linux-x86_64.tar.gz /
RUN cd / && tar xzvf pid1*.tar.gz && rm /pid1*.tar.gz

RUN apt-get update && \
    apt-get install -y libpq5 && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV POSTGREST_VERSION 0.4.0.0
