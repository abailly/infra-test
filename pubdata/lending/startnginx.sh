#!/bin/bash
set -x
set -e
if [ -f /home/build/.app.cid ]; then
    docker kill $(cat /home/build/.app.cid)
    rm /home/build/.app.cid
fi
# run as build user
docker run -d --cidfile=/home/build/.app.cid -p 8080:8080 capital/app:latest
if [ -f /home/build/.nginx.cid ]; then
   docker kill $(cat /home/build/.nginx.cid)
   rm /home/build/.nginx.cid
 fi
  # clone or pull nginx config as build user
export NGINXCONF=/home/build/nginxconf/nginx
#if [ -d /home/build/nginxconf ]; then
#  cd /home/build/nginxconf && git pull origin master
#else 
#  cd /home/build && git clone capital-match nginxconf
#fi
docker run -d --cidfile=/home/build/.nginx.cid -p 80:80 -p 443:443 -v $NGINXCONF/sites-enabled:/etc/nginx/sites-enabled -v /etc/nginx/certs:/etc/nginx/certs -v $NGINXCONF/logs:/var/log/nginx capital/nginx
