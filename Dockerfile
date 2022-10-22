FROM ruby:2.7
# define the environment variable install path
ENV INSTALL_PATH /opt/app/
# copy files from ruby folder into the container
COPY ruby/ $INSTALL_PATH
# install RSpec
RUN gem install rspec
# set the working directory
WORKDIR $INSTALL_PATH