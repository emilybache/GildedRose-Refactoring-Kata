FROM composer as composer

FROM php:8.2-fpm-bullseye as php-base

RUN set -xe; \
    apt-get update -yqq && \
    pecl channel-update pecl.php.net && \
    apt-get install -yqq \
      apt-utils \
      gnupg2 \
      git

###########################################################################
# Zip:
###########################################################################
RUN apt-get install -yqq \
    libzip-dev zip unzip && \
    docker-php-ext-configure zip && \
    # Install the zip extension
    docker-php-ext-install zip && \
    php -m | grep -q 'zip'

###########################################################################
# GD:
###########################################################################
RUN apt-get install -yqq libpng-dev && \
    docker-php-ext-configure gd \
          --prefix=/usr \
          --with-jpeg \
          --with-webp \
          --with-xpm \
          --with-freetype; \
    docker-php-ext-install gd

###########################################################################
# EXIF:
###########################################################################
RUN docker-php-ext-install exif;
RUN docker-php-ext-enable exif;

###########################################################################
# Imagick:
###########################################################################
RUN apt-get install -y libmagickwand-dev; \
    pecl install imagick; \
    docker-php-ext-enable imagick;

###########################################################################
# PHP GETTEXT EXTENTION:
###########################################################################
RUN docker-php-ext-install gettext;
RUN docker-php-ext-enable gettext;

###########################################################################
# PHP GETTEXT EXTENTION:
###########################################################################
RUN docker-php-ext-install calendar && docker-php-ext-configure calendar

###########################################################################
# Human Language and Character Encoding Support:
###########################################################################
RUN apt-get install -yqq zlib1g-dev libicu-dev g++ && docker-php-ext-configure intl && docker-php-ext-install intl && docker-php-ext-enable intl

###########################################################################
# APCU:
###########################################################################
ARG INSTALL_APCU=false
RUN if [ ${INSTALL_APCU} = true]; then \
  pecl install apcu \
  && docker-php-ext-enable apcu \
;fi

###########################################################################
# OPCACHE:
###########################################################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ]; then \
    docker-php-ext-install opcache \
;fi
# Copy opcache configration
COPY ./conf.d/opcache.ini /usr/local/etc/php/conf.d/opcache.ini

###########################################################################
# PHP REDIS EXTENSION
###########################################################################
ARG INSTALL_PHPREDIS=false
RUN if [ ${INSTALL_PHPREDIS} = true ]; then \
  pecl install -o -f redis \
  && rm -rf /tmp/pear \
  && docker-php-ext-enable redis \
;fi

############################################################################
# Xdebug
# NOTE: Please do not install xdebug and PCOV at the same time
############################################################################
ARG INSTALL_XDEBUG=false
RUN if [ ${INSTALL_XDEBUG} = true ]; then \
    pecl install -o -f xdebug \
;fi

# Copy xdebug configration
COPY ./conf.d/xdebug.ini /usr/local/etc/php/conf.d/xdebug.ini

###########################################################################
# MySQL
###########################################################################
ARG INSTALL_MYSQL=false
RUN if [ ${INSTALL_MYSQL} = true ]; then \
  docker-php-ext-install pdo_mysql \
;fi

###########################################################################
# RABBITMQ:
###########################################################################
ARG INSTALL_AMQP=false
RUN if [ ${INSTALL_AMQP} = true ]; then \
    docker-php-ext-install sockets \
    && apt-get install -yqq librabbitmq-dev \
    && pecl install amqp-1.11.0 \
    && docker-php-ext-enable amqp \
    && php -m | grep -oiE '^amqp$' \
;fi

ARG INSTALL_YAML=false
RUN if [ ${INSTALL_YAML} = true ]; then \
    apt-get install -yqq libyaml-dev \
    && pecl install yaml \
    && docker-php-ext-enable yaml \
;fi

COPY ./conf.d/core.ini /usr/local/etc/php/conf.d/core.ini

# Configure non-root user.
ARG PUID=1000
ENV PUID ${PUID}
ARG PGID=1000
ENV PGID ${PGID}

RUN groupmod -o -g ${PGID} www-data && \
    usermod -o -u ${PUID} -g www-data www-data

WORKDIR /var/www

FROM php-base AS php-fpm

USER www-data

EXPOSE 9000

CMD ["php-fpm"]

FROM php-base AS php-cli

COPY --from=composer /usr/bin/composer /usr/bin/composer

###########################################################################
# PCOV
###########################################################################
ARG INSTALL_PCOV=false
RUN if [ ${INSTALL_PCOV} = true ]; then \
  pecl install pcov \
  && docker-php-ext-enable pcov \
;fi

USER www-data

CMD ["php-fpm"]
