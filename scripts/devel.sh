set -ex

stack install :deadpager-server --file-watch --exec='./scripts/redo.sh' --flag deadpager-server:dev
