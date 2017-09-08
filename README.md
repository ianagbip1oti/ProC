# ProC

[![Build Status](https://travis-ci.org/ianagbip1oti/ProC.svg?branch=master)](https://travis-ci.org/ianagbip1oti/ProC)
[![Coverage Status](https://coveralls.io/repos/github/ianagbip1oti/ProC/badge.svg?branch=master)](https://coveralls.io/github/ianagbip1oti/ProC?branch=master)

Hawt new programming language

## Requirements

Initial list as provided by [@InitializeSahib](http://github.com/InitializeSahib)

- c-like syntax (but can deviate a little)
- no do/end stuff (edited)
- no case-based syntax (looking at you, go)
- no -> operator
- native string type
- should be simpler than C (e.g. not ASM)
- optional: actual module system
- optional: package manager for modules

Additional requirements upon further discussion:

- Static Typing
- Compiled (Note, I believe this will be possible to add, even with initially being an interpreted language)

## Build and Run

### Stack

```bash
> stack build
> stack exec ProC examples/hello_world.proc
```
