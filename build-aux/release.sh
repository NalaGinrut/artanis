#!/bin/bash

if [ -z "$1" ]
then
   echo "Release version is missing, now is `git describe`"
   exit -1;
fi

git tag -a -m"release $1" v"$1"
./autogen.sh
./configure
make prepare
make announce

echo "\n======================================"
echo "Now run 'make upload'"
echo "Then 'git push origial all --tags'"
echo "======================================\n"