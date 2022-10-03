#!/bin/bash

HERE=$(pwd)
cd ~/plutus-apps
nix-shell --command "code $HERE/NativeTokens"

