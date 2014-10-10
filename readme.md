Haskell Web seed
================

Starter app for web development
- Logging with [fast-logger](http://www.yesodweb.com/blog/2014/01/new-fast-logger)
- Static assets with wai-app-static
- Caching with Redis
- AWS service integration

Prerequisites
-------------

1. `ghci` and `cabal`. Get them from the [Haskell platform](http://www.haskell.org/platform)
1. (optional) [Redis](http://redis.io/download)
1. (optional) [Amazon Web Services](http://aws.amazon.com/developers/access-keys/)

Running the project
-------------------

### Do this once

1. clone this repo `git clone git@github.com:phylake/haskell-web-seed.git`
1. `cd haskell-web-seed`
1. install dependencies `cabal install --only-dependencies`

### Do this everytime

Run `ghci` like this `ghci main.hs`

Turn options on with `-D` flags. For example to activate code inside `#ifdef DEBUG ... #endif` turn the `DEBUG` flag on like this `ghci -DDEBUG main.hs`

#### Flag list

- `DEBUG`
- `USE_REDIS`

Feedback
--------

Please [open an issue](https://github.com/phylake/haskell-web-seed/issues/new) if you have improvements/cross-references/additions you'd like to see

Look for more instructions and documentation in the source.
