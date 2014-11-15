**Obsolete, the commandline tool has been merged into [puzzle-draw][puzzle-draw]**.

puzzle-draw-cmdline
===================

Rendering logic puzzles, command-line support.

See [puzzle-draw][puzzle-draw] for the main part of this project.

This repository was split out to move the dependency
on cairo out of puzzle-draw.

To enable the Cairo backend (required for png/pdf/ps output), use
    $ cabal install -f cairo

[puzzle-draw]: http://github.com/robx/puzzle-draw
