LIBS = str unix lablgtk
INCDIRS=+lablgtk2
SOURCES = util.ml config.ml shape.ml timer.ml pit.ml state.ml gui.ml main.ml
RESULT  = tetris
debug: debug-code
opt: native-code
profile: profiling-byte-code
all: byte-code
top: top
-include OCamlMakefile
