set -ex

cd deadpager-server
ghcid --command 'stack repl --ghc-options="-O0"' \
  --test=":main --config ../scripts/config.yaml" \
  --restart=templates \
  --restart=scripts \
  --restart=config \
  --restart=package.yaml

