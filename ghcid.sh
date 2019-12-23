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
  --warnings \
  --command "stack ghci Day3.hs" \
  --test="selfTest" $@
