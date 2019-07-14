#!/bin/bash

set -x

killall deadpager-server || true

set -e

cd deadpager-server

deadpager-server &
