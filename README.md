[![CI](https://github.com/moosichu/zar/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/moosichu/zar/actions/workflows/ci.yml)

This repo contains an in-progress attempt to write an [archiver](https://en.wikipedia.org/wiki/Ar_(Unix)) in [zig](https://github.com/ziglang/zig) in order to solve [this issue](https://github.com/ziglang/zig/issues/9828).

## Current Status

So the project did/has reached a state where it works as a drop-in replacement for llvm ar when used as an archiver when building redis on MacOS/Linux. Sometimes zig updates cause regressions which need to fixed.

CI has been setup for this.

## Next steps

The next steps are to expand the testing suite (leveraging the testing harness that zld uses) to confirm zig ar can work as an llvm ar replacement under a wider set of circumstances (culminating in being used with the self-hosted compiler).

The current basic test-suite just compares the output of zig ar with llvm ar and validates the the results are byte-for-byte identical, however this isn't really ideal and probably stricter than what is needed.

## Contributing

If you would like to contribute please do let me know! Progress on this project has be non-linear and I have busy periods, but if you ping an issue or DM me on Discord (I'm on the [zig server](https://discord.gg/zig]), I will always aim to be responsive.

## Other notes

The thing that make this tricky is that the "ar" format used as a basis for the archives that linkers use hasn't been standardised, meaning that depending on the platform being targeted - the format can vary in subtly different ways.

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
