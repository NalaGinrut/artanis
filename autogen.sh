#!/bin/sh
# Usage: sh -x ./autogen.sh

set -e

[ -f ARTANIS-VERSION ] || {
  echo "autogen.sh: run this command only at the top of Artanis."
  exit 1
}

######################################################################
### announce build tool versions
echo ""
autoconf --version
echo ""
automake --version
echo ""
libtoolize --version
echo ""
${M4:-m4} --version
echo ""
flex --version
echo ""

######################################################################
### update infrastructure

autoreconf -i --force --verbose

echo "Now run configure and make."
