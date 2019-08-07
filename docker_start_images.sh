#!/bin/sh

docker load -i lwm-backend-images.tar
docker-compose stop
docker-compose down
docker-compose up -d

if [ $? -eq 0 ]; then
    echo docker is up
else
    echoerr docker compose failed
fi