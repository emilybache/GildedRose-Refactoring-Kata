FROM postgres:12.1 as base
WORKDIR /app

ENV PGHOST=localhost
ENV PGDATABASE=kata
ENV PGPASSWORD=admin
ENV PGUSER=postgres
ENV POSTGRES_PASSWORD=admin
ENV PGDATA /var/lib/postgresql/data_local

RUN mkdir -p "$PGDATA" && chown -R postgres:postgres "$PGDATA" && chmod 777 "$PGDATA"

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates wget \
 && rm -rf /var/lib/apt/lists/*

ADD ./*.sh /app/
ADD ./src /app/src

ADD ./pgunit/initialize.sh /app/
ADD ./pgunit/*.sql /app/
RUN chmod +x ./*.sh \
 && ./initializeDocker.sh
