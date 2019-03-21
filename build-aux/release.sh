#!/bin/bash

git tag -a -m"release $1" v"$1"
./autogen.sh
./configure
make prepare
make announce
