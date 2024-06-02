#!/bin/zsh

rm -rf checked-out
mkdir -p checked-out
cp -r ../proj checked-out/proj
#-- clear history
# rm -rf checked-out/???/.git
