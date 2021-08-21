# 1: Build the exe
FROM rust:latest as builder
WORKDIR /usr

# 1a: Prepare for static linking
RUN apt-get update
RUN apt-get dist-upgrade -y
RUN apt-get install -y musl-tools
RUN curl -sL https://deb.nodesource.com/setup_14.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install nodejs
    # Install elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/
    # Install elm-spa
RUN npm install -g elm-spa@latest
USER root
RUN rustup override set nightly
RUN rustup target add x86_64-unknown-linux-musl

# 1b: Copy files into builder container and create volume to be able to copy files off builder container
COPY /server /usr/server/
COPY /src /usr/elm/src
COPY /public /usr/elm/public
COPY /elm-simple-animation /usr/elm/elm-simple-animation
COPY elm.json /usr/elm/elm.json
COPY version_append.sh /usr/elm/version_append.sh
RUN mkdir /compile-path
RUN mkdir /compile-path/server
RUN mkdir /compile-path/elm

# 1c: Download and compile Rust dependencies (and store as a separate Docker layer)
WORKDIR /usr/server
#RUN cargo install --target x86_64-unknown-linux-musl --path .

# 1d: Build the exe using the actual source code
#RUN cargo install --target x86_64-unknown-linux-musl --path .
ARG ADMINS
RUN cargo build --release
RUN cp ./target/release/elm-spa_server /compile-path/server/ -r

#2: Build the elm project
WORKDIR /usr/elm
RUN elm-spa build
RUN bash version_append.sh
RUN cp ./public /compile-path/elm/ -r
    # Changing the volume from within the Dockerfile: If any build steps change the data within the volume after it has been declared, those changes will be discarded. Thus:
VOLUME /compile-path

# 3: Copy the exe and extra files ("static") to an empty Docker image
#
# // This docker container works with `FROM scratch` to save on image size, though using google cloud run requires that the port be $PORT
# // as seen in `CMD ROCKET_PORT=$PORT`, but I have not figured out how to either compile rocket in a way or to add an enironment variable
# // to a scratch image. Hopefully I will figure that out eventually
FROM ubuntu:latest
RUN apt-get update && apt-get install libpq5 -y
COPY --from=builder /compile-path/server /server
COPY --from=builder /compile-path/elm/public /server/public
RUN ls /server
USER 1000
CMD ROCKET_PORT=$PORT ./server/elm-spa_server
