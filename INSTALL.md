throughIn the current version, Tetris requires the ocaml-canvas and bogue packages to display the gui. This can be installed through opam:
```
opam install ocaml-canvas bogue
```
Note that ocaml-canvas has some dependencies that opam sometimes does not take care of, if you receive an error when installing or get are missing headers when trying to run the gui, try downloading the following packages with your system's package manager:
```
fontconfig-devel freetype-devel libxcb-devel libxkbcommon-devel xcb-util-image-devel xcb-util-keysyms-devel
```

```
dune build
dune exec bin/main.exe
```

Alternatively, to run the terminal-based version, you can run
```
dune build
dune exec bin/term.exe
```
