#!/bin/sh

img_name=lwm-backend
packed_img_name=${img_name}.tar

buildDockerImage() {
  docker image rm ${img_name}
  docker build -t ${img_name} .
}

packBackend() {
  echo packing image...
  docker save -o ${packed_img_name} ${img_name}
  echo image packed
}

clearDockerImages() {
  docker-compose stop &&
    docker-compose down &&
    docker image rm ${img_name}
  docker image prune -f
}

uploadToServer() {
  echo uploading to server...

  scp ${packed_img_name} $1 &&
    rm ${packed_img_name} &&
    echo image uploaded
}

case "$1" in
"local")
  clearDockerImages &&
    buildDockerImage &&
    docker-compose up -d &&
    exit 0
  ;;
"stage")
  clearDockerImages &&
    buildDockerImage &&
    packBackend &&
    uploadToServer $2 &&
    exit 0
  ;;
*)
  echo expected stage or local, but was $1
  exit 1
  ;;
esac