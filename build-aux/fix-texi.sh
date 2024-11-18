#!/bin/bash


# remove @node Top and @top GNU Artanis web-framework Manual from docs/artanis.texi
sed -i '/^@node Top/d' docs/artanis.texi
sed -i '/^@top GNU Artanis web-framework Manual/d' docs/artanis.texi
