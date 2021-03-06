#!/usr/bin/env nix-shell
#!nix-shell -p jq -i bash

nix-build https://github.com/nixos/nixpkgs/archive/4fb198892d298452023ab176e7067da58d30772e.tar.gz -A hydra
echo '~~~ Evaluating release.nix'
command time --format '%e' -o eval-time.txt ./result/bin/hydra-eval-jobs -I . release.nix > eval.json
EVAL_EXIT_CODE="$?"
if [ "$EVAL_EXIT_CODE" != 0 ]
then
  rm eval.json eval-time.txt
  echo -e "\\e[31;1mERROR: Failed to evaluate release.nix\\e[0m"
  exit 1
fi
EVAL_TIME=$(cat eval-time.txt)
jq . < eval.json
ERRORS=$(jq -r 'map_values(.error)|to_entries[]|select(.value)|@text "\(.key): \(.value)"' < eval.json)
NUM_ERRORS=$(jq -r '[ map_values(.error)|to_entries[]|select(.value) ] |length' < eval.json)
rm eval.json eval-time.txt

if [ "$NUM_ERRORS" != 0 ]
then
  echo -e "\\e[31;1mERROR: evaluation completed in $EVAL_TIME seconds with $NUM_ERRORS errors\\e[0m"
  echo "$ERRORS"
  exit 1
else
  echo -e "\\e[32;1mOK: evaluation completed in $EVAL_TIME seconds with no errors\\e[0m"
  exit 0
fi
