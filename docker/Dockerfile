FROM ruby:alpine

LABEL description="Alpine + Ruby + Hugo Docker container."
LABEL maintainer="Peter Clark <peter.clark@tiot.jp>"

# taken from jojomi/docker-hugo at https://github.com/jojomi/docker-hugo/blob/0.25.1/Dockerfile
COPY ./run.sh /run.sh
RUN chmod u+x /run.sh

ENV HUGO_VERSION=0.25.1
#ENV HUGO_VERSION=0.29
#ENV HUGO_VERSION=0.39
#ENV HUGO_VERSION=0.49
#ENV HUGO_VERSION=0.59.1
#ENV HUGO_VERSION=0.69.0
#ENV HUGO_VERSION=0.79.1
#ENV HUGO_VERSION=0.85
ENV HUGO_PORT=1314
ADD https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_Linux-64bit.tar.gz /tmp
RUN tar -xf /tmp/hugo_${HUGO_VERSION}_Linux-64bit.tar.gz -C /tmp \
    && mkdir -p /usr/local/sbin \
    && mv /tmp/hugo /usr/local/sbin/hugo \
    && rm -rf /tmp/hugo_${HUGO_VERSION}_linux_amd64
RUN apk add --no-cache ca-certificates

# add stuff to allow ruby to compile native extentions
RUN apk -U add --no-cache gcc musl-dev make g++ git build-base clang \
    && rm -rf /var/cache/apk/*
ENV CXX=clang++

# add stuff to make mini_racer work
RUN gem install 'specific_install'
#RUN gem specific_install -l "https://github.com/sagarjauhari/mini_racer"

# pre-install commonnly used items
#RUN gem install sass
#RUN gem install autoprefixer-rails
#RUN gem install sprockets -v '~>3.7'
#RUN gem install coffee-script
#RUN gem install uglifier
#RUN gem install rake
#RUN gem install guard
#RUN gem install guard-rake

#RUN gem install pry
#RUN gem install pry-byebug
#RUN gem install net-sftp

#RUN gem install mini_racer

VOLUME /src
VOLUME /output

WORKDIR /src
CMD ["/run.sh"]

EXPOSE ${HUGO_PORT}
