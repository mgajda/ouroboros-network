#!/bin/bash

ghcid -c "stack ghci latency-sim:latency-sim-test" --test Main.main --restart src/ --restart test/ --restart src/*.lhs
