# Tak-Alike

A game like [Tak](<https://en.wikipedia.org/wiki/Tak_(game)>)

## Goal

Create a path of your stones and/or capstones between 2 opposite edges.

## Gameplay

On your turn, take 1 of 2 actions
- Places a stone, wall, or capstone on any empty space
- Move a stack of pieces to an adjacent space
    - You can't move on top of a wall, except with a capstone. This turns the wall into a stone
    - You can't move more pieces in a stack than the size of the board. I.e. if the board is of size 3, and you have a stack of 5 pieces, you may only move 1-3 of the top pieces.

## Development

### Getting Started

Clone this repo. Make sure you've installed

- [Lamdera](https://lamdera.com/)
- [Node](https://nodejs.org)

then run

```sh
npm install
npm run dev
```

### Features

- [Lamdera](https://lamdera.com/) as the platform
- [@ryannhg/css-in-elm](https://www.npmjs.com/package/@ryannhg/css-in-elm) for CSS code generation
- [elm-format](https://github.com/avh4/elm-format) for code formatting
- [elm-review](https://www.npmjs.com/package/elm-review) for code review and cleanup
- [run-pty](https://www.npmjs.com/package/run-pty) for running the dev environment in a clean and easy to read manner
