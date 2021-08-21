FROM node:latest

COPY package.json .
RUN npm install elm@elm0.19.0
RUN npm install http-server

COPY elm.json .
COPY . .

ENV PUBLIC_URL https://xxx.herokuapp.com

RUN chmod 777 Makefile
RUN make

CMD http-server -p $PORT dist
