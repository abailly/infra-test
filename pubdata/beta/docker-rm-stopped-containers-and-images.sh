#!/bin/bash
docker ps -a | awk '{print $1}' | xargs --no-run-if-empty docker rm
#remove all unused images, except those tagged as latest, to speed up the first build after cleaning
docker images | grep latest | awk '{ printf("%s\n",$3) }' | sort > latest.txt && cat latest.txt
docker images | grep -v IMAGE | awk '{ printf("%s\n",$3) }' | sort > all.txt && cat all.txt
comm -3 all.txt latest.txt | docker rmi

#restart ci
cd /home/build/ci
fig stop
rm -rf /home/build/.ci/* # get rid of *.uuid files
fig build
fig up -d

