#!/bin/bash -ex

rm -rf ../oreilly/images
mkdir ../oreilly/images
runghc to-docbook.hs
cp yesod.xml ../oreilly/book.xml
cd book/yesod-web-framework-book
for f in $(find . -name \*.png)
do
    mkdir -p ~/haskell/oreilly/images/$(dirname $f)
    cp $f ~/haskell/oreilly/images/$f
done
