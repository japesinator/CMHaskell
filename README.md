# CMHaskell
CMHaskell is an implementation of the C Minus language, implemented in Haskell. I wrote it with the intent of teaching myself
both Haskell and compiler construction.

#Design
CMHaskell does lexing and parsing in one step(recursive descent) and emits a Generalized Abstract Data Type(GADT) that represents the 
AST of a parsed program.
It emits 32-bit x86, gas-syntax assembly code. The language specification was taken from this source:

http://www.cs.dartmouth.edu/~cs57/Project/C-%20Spec.pdf 

The code generation follows a very simple stack-based tiling strategy taken from:

http://www.cs.usfca.edu/~galles/compilerdesign/x86.pdf

I pretty much skipped over semantic analysis, and did no optmiziations. Perhaps I will revisit the issue in the future.

# How to Build
CMHaskell has a dependency on Parsec, which can be acquired through cabal with:

cabal install parsec

You should then be able to do:

ghc Driver.hs -o Driver 

Which will compile the driver executable.

For convenience, I use the C library functions printf() and scanf() to implement input() and output(). As such, the output assembly
file must be linked against a 32-bit C library. This is usually accomplished with a command such as:

sudo apt-get install gcc-multilib

#How to Use
./Driver \<inputFile\> \<outputFile\>

Will generate a 32-bit x86 assembly file.

gcc -m32 \<outputFile\> -o \<executable\>

Will compile and link the output assembly into an executable. You probably need to specifically use gcc, as the while and if
statement code generators use automatic local labels, which are a feature of gas.

