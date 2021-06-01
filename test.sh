#!/usr/bin/env bash

cabal build GetTCTraces &&
./dist/build/GetTCTraces/GetTCTraces --trades test/test1.txt > test/sink &&
diff -U -1 test/test1_res.txt test/sink | colordiff | diff-highlight
