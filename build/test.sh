#!/bin/sh -e

tests=$(find . -name 'test_*' -executable)
exec_dir=$(pwd)

cd ../tests
for i in $tests; do
    $exec_dir/$i
done
cd -
