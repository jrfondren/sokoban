# sokoban
A toy implementation of Sokoban. Mostly an exercise in dependency injection
with OCaml effect handlers. You are `@`. Push the blocks `$` into the goals
`.` without blocking yourself from this goal.

# bare UI
```
$ dune utop
utop # Sokoban.Bare_tui.play ();;

 #####
 #.. #
###  #
# $  #
# $ ##
#@  #
#####
wasd, e/h, q?
```

All the UI is standard I/O. Enter a line of just 'w' to move up.

# Terml UI
```
$ dune exec sokoban
```

You can now move with the arrow keys.
