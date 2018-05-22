# rpn

A reverse polish notation calculator I wrote to get a little more into the
Haskell language. The whole thing can be built using `stack`. By running

```
stack install
```

a binary called `rpn` will be installed into your predefined stack binary
directory.

Currently, the calculator will read everything from `stdin` and try to
parse it as a reverse polish notation 'program'. So, the simplest usage
is as follows:

```
echo "1 2 +" | rpn
```

(Assuming you have `rpn` / the stack binary directory in your PATH).
By default, the calculator ends with printing the topmost value in the
calculation stack.

