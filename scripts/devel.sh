set -ex

stack install :deadpage-server --file-watch --exec='./scripts/redo.sh' --flag deadpage-server:dev
