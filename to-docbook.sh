#!/bin/bash -ex

rm -rf ../oreilly/images
mkdir ../oreilly/images
runghc to-docbook.hs
cp yesod.xml ../oreilly/book.xml
cd book/yesod-web-framework-book
for f in $(find . -name \*.png)
do
    mkdir -p ../oreilly/images/$(dirname $f)
    cp $f ../oreilly/images/$f
done
