# HOME

This is an opinionated keyboard layout designed to centralize actions to the homerow.

## Design

<!-- BEGIN:keymap_table -->
<!-- END:keymap_table -->

## Executable

`home` is the MacOS exe which intercepts and forwards key-events, identifying and handling chords.

## TUI

`home-tui` is a helper application which displays the keys/chords to press for the next letter to exercise.

![tui](https://github.com/user-attachments/assets/d9ff1e92-8008-464b-9a05-66c68bce6ad1)

## Build

Before `cabal run` go into the `c_lib` directory and do:

```sr
make build
```
