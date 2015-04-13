#!/bin/bash
docker ps -a | awk '{print $1}' | xargs --no-run-if-empty docker rm
docker images | awk '{print $3}' | xargs --no-run-if-empty docker rmi

#restart ci
cd /home/build/ci
fig stop
fig build
fig up -d
i
