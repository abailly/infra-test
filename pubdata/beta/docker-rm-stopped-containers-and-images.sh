#!/bin/bash
docker ps -a | awk '{print $1}' | xargs --no-run-if-empty docker rm
#remove all unused images, except those tagged as latest, to speed up the first build after cleaning
docker images | sed '/latest/d' | awk '{print $3}' | xargs --no-run-if-empty docker rmi

#restart ci
cd /home/build/ci
fig stop
rm -rf /home/build/.ci/* # get rid of *.uuid files
fig build
fig up -d

