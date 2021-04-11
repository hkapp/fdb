#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 <ghc-option>"
  echo ""
  echo "To pass multiple arguments, use quotes:"
  echo "ex: $0 \"-a -b\""
  exit 1
fi

dest_dir=`dirname $0`
new_options="$1"
basic_options="-XMultiParamTypeClasses -no-hs-main -no-link -fforce-recomp"

# If multiple options, concat into one
file_ext=${new_options// /}
# Replace first '-' by '.'
file_ext=${file_ext/-/.}
# Now replace all remaining '-' by '_'
file_ext=${file_ext//-/_}

hs_filepath="$dest_dir/../TPCH/Functional/Q1.hs"
hs_filename=`basename $hs_filepath`

out_file="$dest_dir/${hs_filename%.hs}$file_ext"

ghc $basic_options $new_options $hs_filepath > $out_file

echo "Generated output to $out_file"

# See:
#  https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html
#  https://serokell.io/blog/haskell-to-core

# Outcome:
#  get DS definition from -ddump-hi
#  get code from -ddump-prep
