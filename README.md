This repo contains an in-progress attempt to write an [archiver](https://en.wikipedia.org/wiki/Ar_(Unix)) in [zig](https://github.com/ziglang/zig) in order to solve [this issue](https://github.com/ziglang/zig/issues/9828).

As of writing no actual work has been done yet and things are just getting started...

The thing that make this tricky is that the "ar" format used as a basis for the archives that linkers use hasn't been standardised, meaning that depending hon the platform being targeted - the format can vary in subtly different ways.

Three main variants to focus on (for now):

 - BSD (32 & 64 Bit) (the latter used on Darwin)
 - System V (or GNU) (32 & 64 BIT)
 - Windows

The goal is to make something that is a drop-in replacement for llvm-ar

The documentation for llvm-ar can be found here:

https://llvm.org/docs/CommandGuide/llvm-ar.html

The Wikipedia article on the format here:

https://en.wikipedia.org/wiki/Ar_(Unix)


A lot of the important operations supported by llvm-ar aren't actually documented in the program itself. However, as it is compatible with other ar implementations a reference man page for one of those can be found here:

https://man7.org/linux/man-pages/man1/ar.1.html
