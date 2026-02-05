setup:
    bun install
    bun spago install

clean_javascript:
    rm -rf node_modules

clean_purescript:
    rm -rf .spago output

clean_client:
    rm -rf client

clean_all: clean_client clean_purescript clean_javascript

test:
    bun spago test

bundle_purescript:
    bun spago bundle --platform browser --source-maps --minify --outfile=purescript_dist/main.js

bundle_client:
    bun run script/build.ts

build: bundle_purescript bundle_client
