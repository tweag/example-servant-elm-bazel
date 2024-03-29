# Haskell Servant and Elm Example

This example project illustrates how to set up a project that:

- Uses haskell and `servant` as a backend exposing a JSON api
  (and serving some files).
- Uses elm to write the frontend.
- Uses `servant-elm` to generate client functions in elm for the JSON api.
  This means that mismatches regarding the JSON api will be detected statically.
- Allows a very fast development cycle: You can type-check the server and
  client code in a very short amount of time.

## Bazel

Enter the provided nix shell and execute any of following targets:
- `:spec` - the backend test suite target
- `:server` - backend server target; web UI is exposed under localhost:3000
- `//client:spec` - the frontend test suite target
- `:generate-elm` - elm binding generation target
- `:api` - library target

### gazelle_cabal targets

- `:gazelle` - creates `BUILD.bazel` file with targets generated from cabal file
- `:gazelle-update-repos` - updates `stack_snapshot` repository with dependencies taken from the existing Bazel targets

## ~~Makefile~~

~~There's a `Makefile` included with the following targets:~~

- ~~`setup` -- Set up everything: install ghc and dependencies. (Needs `stack`, `elm`
  and `elm-test`.)~~
- ~~`build` -- Build the server and the client.~~
- ~~`server-start` -- Start the server here: <http://localhost:3000/>. Requests sent
  to this server will trigger a recompilation of the client code (not the server
  code).~~
- ~~`test` -- Recompiles the client and server code. And runs the test-suite.~~

## Caveats

- This project uses <https://travis-ci.org/soenkehahn/wai-make-assets>, which is
  experimental. E.g. there's no support for serving the assets in a production setting.

## More examples

This repository demonstrates a servant backend and an elm frontend. More code generation examples can be found in [`haskell-servant/servant-elm`](https://github.com/haskell-servant/servant-elm/tree/master/examples).
