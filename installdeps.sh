#!/bin/sh

cabal-dev install -f highlighting pandoc
cabal-dev install --only-dependencies
