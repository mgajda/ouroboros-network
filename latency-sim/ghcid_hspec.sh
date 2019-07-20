#!/bin/bash

ghcid -c "stack ghci latency-sim:latency-sim-test" \
      --test Main.main \
      --restart test/  \
      $(for litfile in src/*.lhs; do echo --restart $litfile; done)

