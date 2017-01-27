# purescript-halogen-virtual-dom

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-halogen-virtual-dom.svg)](https://github.com/slamdata/purescript-halogen-virtual-dom/releases) [![Build status](https://travis-ci.org/slamdata/purescript-halogen-virtual-dom.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen-virtual-dom)

A [virtual-dom](https://github.com/Matt-Esch/virtual-dom) driver for [Halogen](https://github.com/slamdata/purescript-halogen).

## Installation

```
bower install purescript-halogen-virtual-dom
```

### :warning: Note :warning:

You will also need to add `virtual-dom` as an NPM dependency to your project, and then use webpack or browserify to bundle it as part of the resulting JS file. If you're building with `pulp` then this is easy to do with the `pulp browserify` command.

## Documentation

The main function of interest is `Halogen.VirtualDOM.Driver.runUI`. A minimal example of its usage:

``` purescript
module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VirtualDOM.Driver (runUI)

main :: Eff (HA.HalogenEffects _) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ?myComponent ?initialInput body
```

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-halogen-virtual-dom).
