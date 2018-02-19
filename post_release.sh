#!/bin/sh
./_build/prod/lib/dderl/post_release.sh egambo

echo `pwd`

cd _build/prod/rel/egambo/lib/egambo-*/priv/dev
rm -rf node_modules
echo "===> dir 'node_modules' deleted"

echo "===> npm install"
npm install

echo "===> npm run build"
npm run build

cd ..
echo "===> dirs 'node_modules' deleted"
rm -rf node_modules
