AHVEN_INCLUDE:=-aI/usr/share/ada/adainclude/ahven -aI/usr/include/ahven
AHVEN_LIBS:=-aO/usr/lib/x86_64-linux-gnu/ada/adalib/ahven -aO/usr/lib/ahven -aO/usr/lib -shared
ADAFLAGS:=-gnat2012 $(AHVEN_INCLUDE) $(AHVEN_LIBS)
LIBS:=-largs -lahven

.PHONY: all
all: gilded_rose_tester program

.PHONY: gilded_rose_tester
gilded_rose_tester:
	@gnatmake -q $(ADAFLAGS) gilded_rose_tester $(LIBS)

.PHONY: test
test: gilded_rose_tester
	@./gilded_rose_tester

.PHONY: program
program:
	@gnatmake -q $(ADAFLAGS) program $(LIBS)

.PHONY: clean
clean:
	@gnatclean -q gilded_rose_tester program
