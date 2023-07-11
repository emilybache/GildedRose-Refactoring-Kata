# Specifies a parent image
FROM golang:1.19.2-bullseye as build
 
# Creates an app directory to hold your app’s source code
WORKDIR /app
 
# Copies everything from your root directory into /app
COPY go.* ./

ENV GOCACHE=/.cache/go-build
ENV GOMODCACHE=/go/mod/pkg
 
# Installs Go dependencies
RUN go mod download

COPY . ./

ENV CGO_ENABLED=0

# We will mark this image with a configurable tag
ARG BUILD_TAG=unknown
LABEL BUILD_TAG=$BUILD_TAG

# Builds your app with optional configuration
RUN go install github.com/swaggo/swag/cmd/swag@v1.16.1
RUN swag init
RUN go build -a -o target/bin/gilded-rose ./main.go



FROM scratch as runtime

WORKDIR /app

# We will mark this image with a configurable tag
ARG BUILD_TAG=unknown
LABEL BUILD_TAG=$BUILD_TAG

COPY --from=build /app/target/bin/gilded-rose /app/target/bin/gilded-rose

# Tells Docker which network port your container listens on
EXPOSE 8080

ENTRYPOINT [ "/app/target/bin/gilded-rose" ]
CMD [ "app:serve" ]
