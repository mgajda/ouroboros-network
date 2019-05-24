#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

set -xe

start-node() {
  cabal new-run demo-playground -- \
        --system-start "$now" \
        --minsev-info \
        --slot-duration 2 \
        node \
        --topology demo-playground/simple-topology.json \
        --real-pbft \
        --node-id $1 \
        &
}

cabal new-build demo-playground

atexit() {
        pkill demo-playground
}
trap atexit EXIT

start-node 0
start-node 1
start-node 2

echo "Press Ctrl-C or Enter to terminate."
read
