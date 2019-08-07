#!/bin/sh

cat $1 | docker exec -i lwm-db psql -U lwm
