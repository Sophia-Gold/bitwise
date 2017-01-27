# bitwise
I found myself writing some bit twiddling functions for projects I was working on and wasn't able to find anything similar in Clojure so decided to put together a library. Most of these (and many more) can be found in the book [Hacker's Delight](http://www.hackersdelight.org/) as well as this collection [here.](http://graphics.stanford.edu/~seander/bithacks.html) This is very much a work in process and I hope to keep refining and adding to it as I have the time.


Functions include:

+ [binary GCD (Stein's Algorithm)](https://en.wikipedia.org/wiki/Binary_GCD_algorithm)
+ bit-shift right for double-precision floats
+ [fast inverse square root](https://en.wikipedia.org/wiki/Fast_inverse_square_root)
+ Log10 of integers
+ XOR swap
+ convert long to binary sequence
+ bit counting (Brian Kernighan's method)
+ reverse bits
+ convert string to upper and lower case
+ simple XOR cipher
+ [Feistel ciper](https://en.wikipedia.org/wiki/Feistel_cipher)
+ half adder, full adder, & ripple-carry adder