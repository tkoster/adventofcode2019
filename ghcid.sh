#!/bin/bash

# Install trap to restore cursor after Ctrl-C.
cnorm() {
  tput cnorm
}
trap cnorm EXIT INT

# Run ghcid.
# - build
# - test
stack exec ghcid -- \
  --command "stack ghci --ghci-options -Wall $1" \
  --test="selfTest"
