set -ex

cd deadpager-server
ghcid --command 'stack repl --ghc-options="-O0"' \
  --test=main \
  --restart=templates \
  --restart=package.yaml

