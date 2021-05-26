FROM ruby:2.6

RUN mkdir -p /app
WORKDIR /app

RUN gem install bundler:2.1.4
COPY Gemfile Gemfile.lock /app/
RUN bundle install

COPY . /app