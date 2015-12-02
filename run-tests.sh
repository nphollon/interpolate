#!/usr/bin/env bash

# Before running this script:
# - Add these dependencies to elm-package.json:
#        "deadfoxygrandpa/elm-test": "2.0.0 <= v < 3.0.0"
#        "laszlopandy/elm-console": "1.0.2 <= v < 2.0.0"
#  - run elm package install

temp_dir=${TMPDIR:-"/tmp"}
elm_out="$temp_dir/test-raw.js"
io_out="$temp_dir/test.js"
io_sh="elm-stuff/packages/laszlopandy/elm-console/1.0.3/elm-io.sh"
main_elm="test/Main.elm"

elm make $main_elm --output $elm_out --yes

if [ $? -ne 0 ]; then exit 1; fi;

bash $io_sh $elm_out $io_out
if [ $? -ne 0 ]; then exit 1; fi;

echo "Successfully generated $io_out"

node $io_out
