# asmh

Very, very incomplete x86_16 assembler for the final project of my Computer Architecture Class.

The assembler was initially written in ~3 days in Haskell, and it specifically targets the `emu8086` emulator, using `emu8086`'s example assembly (found in in this repository: [test/data](test/data)) in the test suite of `asmh`, where the output of `asmh` is compared to the output of the builtin assembler from `emu8086` (which uses [`FASM`](https://en.wikipedia.org/wiki/FASM) internally with a bit of extra preprocessing, though through some testing I verified that [NASM](https://nasm.us/) output is the same). Currently it can fully compile only [test/data/asm/colors.asm](test/data/asm/colors.asm).

## Documentation & resources consulted:

- `emu8086` docs: <https://surendrajat.github.io/emu8086/documentation/>
- x86 Opcode and Instruction Reference: <http://ref.x86asm.net/>
- Manual inspection of binary output of `emu8086` (i.e. `FASM`).

