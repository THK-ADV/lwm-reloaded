#!/bin/sh

echoerr() { echo "$@" 1>&2; }

./docker_build.sh

if [ $? -eq 0 ]; then
  docker-compose stop
  docker-compose down
  docker-compose up -d
  if [ $? -eq 0 ]; then
      echo docker is up
  else
      echoerr docker compose failed
  fi
else
    echoerr docker build failed
fi