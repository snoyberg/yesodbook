#!/bin/bash -ex

rm -rf html tmp
runghc to-html.hs
mv html tmp
mkdir html
mv $(find tmp -name \*.html) html
for f in html/*.html ; do sed -i 's@"\.\./\.\./\.\./images@"images@g' $f ; done
mv tmp/images html
rm -rf tmp
