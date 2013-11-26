.PHONY: all run clean mrproper

all: dbgb.gb

clean:
	@-$(RM) $(wildcard dbgb.o)

mrproper: clean
	@-$(RM) $(wildcard dbgb.gb)



dbgb.o: dbgb.s Makefile
	@wla-gb -o dbgb.s

dbgb.gb: dbgb.o link
	@wlalink link dbgb.gb


run: dbgb.gb
	@mednafen dbgb.gb
