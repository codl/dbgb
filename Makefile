SRCFILES := $(shell find $(PROJDIRS) -type f -name "*.s")

.PHONY: all run clean mrproper

all: dbgb.gb

clean:
	@-$(RM) $(wildcard dbgb.o)

mrproper: clean
	@-$(RM) $(wildcard dbgb.gb)


dbgb.o: $(SRCFILES)
	wla-gb -o dbgb.s

dbgb.gb: dbgb.o linkfile
	wlalink linkfile dbgb.gb


run: dbgb.gb
	mednafen dbgb.gb >/dev/null &
