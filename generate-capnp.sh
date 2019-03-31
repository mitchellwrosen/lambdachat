#!/bin/sh

set -ex

capnp compile -ohaskell:src protocol/message.capnp
