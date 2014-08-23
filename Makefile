EXEC=bfasm

all:
	ocp-build -init
	rm -f ocp-build.root*old
	mkdir -p bin/
	cp _obuild/$(EXEC)/$(EXEC).asm bin/$(EXEC)

clean:
	rm -Rf _obuild/
	rm -f ocp-build.root

mrproper: clean
	rm bin/$(EXEC)

.PHONY: clean mrproper
