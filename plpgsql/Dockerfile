FROM postgres:12.1 as base
WORKDIR /app

ENV PGHOST=localhost
ENV PGDATABASE=kata
ENV PGPASSWORD=admin
ENV PGUSER=postgres
ENV POSTGRES_PASSWORD=admin
ENV PGDATA /var/lib/postgresql/data_local

RUN mkdir -p "$PGDATA" && chown -R postgres:postgres "$PGDATA" && chmod 777 "$PGDATA"

ADD ./*.sh /app/
ADD ./src/item.sql /app/src/
ADD ./src/new_item.sql /app/src/

# PGUNIT
FROM base as pgunit

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates wget \
 && rm -rf /var/lib/apt/lists/*

ADD ./pgunit/initialize.sh /app/
ADD ./pgunit/*.sql /app/
RUN chmod +x ./*.sh \
 && ./initializeDocker.sh

# PGTAP
FROM base as pgtap

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates build-essential git-core libv8-dev curl postgresql-server-dev-12 \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /tmp/pgtap \
 && cd /tmp/pgtap \
 && git clone https://github.com/theory/pgtap.git /tmp/pgtap \
 && make \
 && make install \
 && cpan TAP::Parser::SourceHandler::pgTAP

ADD ./pgtap/initialize.sh /app/
RUN chmod +x ./*.sh \
 && ./initializeDocker.sh
