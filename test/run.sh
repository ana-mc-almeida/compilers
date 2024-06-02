#!/bin/zsh

TEST_TIME=$(date +"%Y%m%d%H%M")

./0-begin.sh "$TEST_TIME"

./1-copy.sh
./2-test.sh
./3-summarize.sh

./4-end.sh "$TEST_TIME"

