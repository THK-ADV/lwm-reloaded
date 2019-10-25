#!/bin/sh

echoerr() { echo "$@" 1>&2; }

buildFolder=target/universal
name=lwm-reloaded-1.0-SNAPSHOT
img_name=lwm-backend

rm -rf ${buildFolder}
sbt dist

if [ $? -eq 0 ]; then
    cd ${buildFolder}
    unzip ${name}.zip
    cd ../..
    docker image rm ${img_name}
    docker build -t ${img_name} .
    if [ $? -eq 0 ]; then
        echo docker build is fine
    else
        echoerr docker build failed
    fi
else
    echoerr sbt failed
fi
