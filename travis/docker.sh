#!/bin/bash

# Exit on first error
set -e


save_and_shutdown() {
  # save built for host result
  # force clean shutdown
  halt -f
}

# make sure we shut down cleanly
trap save_and_shutdown EXIT SIGINT SIGTERM

# Start docker daemon
docker -d &
sleep 5

docker build -t time-attack .
