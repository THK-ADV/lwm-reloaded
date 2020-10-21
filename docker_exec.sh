#!/bin/sh

buildFolder=target/universal
name=lwm-reloaded-1.0
img_name=lwm-backend
packed_img_name=${img_name}.tar

buildApp() {
  rm -rf ${buildFolder}
  sbt dist
}

buildDockerImage() {
  cd ${buildFolder}
  unzip ${name}.zip
  cd ../..
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

deployDockerImages() {
  docker load -i ${packed_img_name} &&
    docker-compose up -d
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
    buildApp &&
    buildDockerImage &&
    docker-compose up -d
  ;;
"stage")
  clearDockerImages &&
    buildApp &&
    buildDockerImage &&
    packBackend &&
    uploadToServer $2
  ;;
*)
  echo expected stage or local, but was $1
  exit 1
  ;;
esac