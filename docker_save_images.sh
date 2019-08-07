#!/bin/sh

docker save -o lwm-backend-images.tar $(docker-compose config | awk '{if ($1 == "image:") print $2;}' ORS=" ")