#!/bin/bash

set -x

killall deadpage-server || true

set -e

cd deadpage-server

deadpage-server &
