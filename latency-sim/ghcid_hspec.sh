#!/bin/bash

stack build

ghcid -c "stack ghci latency-sim:latency-sim-test" \
      --test Main.main \
      $(for litfile in src/*.lhs; do echo --restart $litfile; done)

