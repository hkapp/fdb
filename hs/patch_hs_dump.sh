#!/bin/bash

f=$1

if [ ! -f "$f" ]; then
  echo "File not found: $f"
  exit 1;
fi

ghcv_text=`ghc --version`
ghcv=`echo "$ghcv_text" | grep -o --color=never '[0-9.]\+'`

if [[ "$ghcv" == "8.0.2" ]] ; then
  sed -i 's/of _ \[Occ=Dead\]/of /' $f
  sed -i 's/sbu\([a-zA-Z]\) ::/sbu\1 [Occ=Once] ::/' $f

  echo "Applied patches for GHC version $ghcv on $f"
  exit 0;
else
  echo "$ghcv_text"
  echo "Unkwown GHC version: $ghcv"
  exit 1;
fi
