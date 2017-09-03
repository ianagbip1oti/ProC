# ProC

Hawt new programming language

## Requirements

- c-like syntax (but can deviate a little)
- no do/end stuff(edited)
- no case-based syntax (looking at you, go)
- no -> operator
- native string type
- should be simpler than C (e.g. not ASM)
- optional: actual module system
- optional: package manager for modules

## Build and Run

### Stack

```bash
> stack build
> stack exec ProC examples/hello_world.proc
```

### Cabal

ProC will be installed to the user installation directory. On *nix this is ~/.cabal/bin.

```bash
> cabal install
> ~/.cabal/bin/ProC examples/hello_world.proc
```

