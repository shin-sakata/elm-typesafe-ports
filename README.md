# elm-typesafe-ports

This npm package improves port data type safety when using Elm and TypeScript.

Generate TypeScript type definitions file by analyzing Elm project.

## Install

```
$ npm install -D elm-typesafe-ports
```

Currently only MacOS is supported.
Linux support when version 1 is released.

## Usage

### Generate TypeScript type definitions file

```
$ npx elm-typesafe-ports {./src/Main.Elm} // <- Elm entry point for your project here
"Success! Generated to ./src/Main.elm.d.ts"
```

After that, just write the functions used in Port in TypeScript

## Current status

In development

## LISENCE
[MIT](./LICENSE)
