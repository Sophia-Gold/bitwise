# bitwise [![Build Status](https://travis-ci.org/Sophia-Gold/bitwise.svg?branch=master)](https://travis-ci.org/Sophia-Gold/bitwise)
I found myself writing some bit twiddling functions for projects I was working on and wasn't able to find anything similar in Clojure so decided to put together a library. Most of these (and many more) can be found in the book [Hacker's Delight](http://www.hackersdelight.org/) as well as [this collection](http://graphics.stanford.edu/~seander/bithacks.html) from Stanford. This is very much a work in process and I hope to keep refining and adding to it as I have the time.


Functions include:

+ [Binary GCD (Stein's Algorithm)](https://en.wikipedia.org/wiki/Binary_GCD_algorithm)
+ Bit-shift right for double-precision floats
+ [Fast inverse square root](https://en.wikipedia.org/wiki/Fast_inverse_square_root)
+ Round to closest power of two
+ Log10 of integers
+ Convert long to binary sequence
+ Least significant bit
+ [Counting bit sets (Brian Kernighan's algorithm)](http://www.geeksforgeeks.org/count-set-bits-in-an-integer/)
+ Counting bit sets (table lookup)
+ Counting bit sets (parallel divide and conquer)
+ Reverse bits
+ Partition string
+ Convert string to upper and lower case
+ [java.lang.String hashCode()](https://en.wikipedia.org/wiki/Java_hashCode())
+ [MurmurHash3](https://en.wikipedia.org/wiki/MurmurHash)
+ Hash combine
+ [Rolling hash](https://en.wikipedia.org/wiki/Rolling_hash)
+ [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter)
+ [Rabin–Karp string searching](https://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm)
+ [XOR swap](https://en.wikipedia.org/wiki/XOR_swap_algorithm)
+ [XOR cipher](https://en.wikipedia.org/wiki/XOR_cipher)
+ [Linear-feedback shift register](https://en.wikipedia.org/wiki/Linear-feedback_shift_register)
+ [Feistel cipher](https://en.wikipedia.org/wiki/Feistel_cipher)
+ [Half adder, full adder, & ripple-carry adder](https://en.wikipedia.org/wiki/Adder_(electronics))
+ [Toffoli gate](https://en.wikipedia.org/wiki/Toffoli_gate)
+ [Fredkin gate](https://en.wikipedia.org/wiki/Fredkin_gate)

---

_To do: S-box, P-box, Shor's algorithm, Grover's algorithm_