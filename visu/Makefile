
CFLAGS = -Wall
LFLAGS = -lSDL
COPTF = -O6 -fomit-frame-pointer -funroll-loops 
CPROF = -O2 -g -pg -fno-inline


all:	visu visu.prof

visu :	visu.c target.o
	$(CC) $(CFLAGS) $(COPTF) $(LFLAGS) -o $@ $^
	strip $@

visu.prof : visu.c target.o
	$(CC) $(CFLAGS) $(CPROF) $(LFLAGS) -o $@ $^

target.o : target.c target.h
	$(CC) $(CFLAGS) -c $^

target.c : target.ppm
	gawk -f prep_target.gawk $^ >$@


clean:
	rm -f visu visu.prof
	rm -f target.c
	rm -f *.orig

