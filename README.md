## Ryan's B Compiler 

#### Outside dependencies:
- ghc
- cabal
- GNU as
- gcc (to link with a C runtime)

Make sure these are findable in your `$PATH`, if these are not installed on your system 
this can be done temporarily by linking to executables in a separate directory and
putting that directory at the front of `$PATH`

Basic usage: `cabal run exe:rbc -- -o [OUTPUT] [FILE]`

Example programs are available in `/programs`

#### Pitfalls:
- Currently only targeting x86-64 Linux
- Unlike in C, a[b] is not always equal to b[a], since words are not 1 byte and
B has no concept of pointer math, as every variable is simply a quadword in memory
- +=, -=, ... operators are reversed from the original spec to make the language 
have one less wart for users used to C, as well as remove ambiguity from parsing
- Instead of `libb.a`, this compiler links with the libc present on a system, meaning 
that \\ is used for escapes instead of \* (as one normally would in C, e.g. `"\n"`)
- Code generation currently focuses on correctness (and conciseness of compiler)
- Code generated is not position independent (emit linked functions with @PLT in future?)

#### Helpful links:
- [Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)
- [B language reference 1](https://www.thinkage.ca/gcos/expl/b/manu/manu.html)
- [B language reference 2](https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html)
