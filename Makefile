all: $(foreach file,$(wildcard src/*.hs),$(subst src,bin,$(basename $(file))))

setup:
	mkdir -p bin $(foreach file,$(wildcard src/*.hs),$(subst src,obj,$(basename $(file))))

bin/%: src/%.hs setup
	ghc -outputdir obj/$(notdir $@) --make -O2 -o $@ $<

clean: FORCE
	rm -r bin obj

FORCE:
