CC=clang
LLC=llc
LLCFLAGS=--march=riscv32
CFLAGS=-S -emit-llvm -fno-discard-value-names --target=riscv32-unknown-elf -O2 -fno-builtin-printf -fno-builtin-memcpy
HOSTCFLAGS=-S -emit-llvm -fno-discard-value-names -O2 -m32

all: builtin.s builtin.x86.ll

builtin.s: builtin.ll
	$(LLC) $(LLCFLAGS) $<

%.ll: %.ll.source
	sed 's/string_/string./g;s/array_/array./g' $< > $@

builtin.ll.source: builtin.c
	$(CC) $(CFLAGS) $< -o $@

builtin.x86.ll.source: builtin.c
	$(CC) $(HOSTCFLAGS) $< -o $@

clean:
	rm -f builtin{,.x86}.{s,ll,ll.source}
