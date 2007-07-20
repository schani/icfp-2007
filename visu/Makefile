
CFLAGS = -Wall -lSDL
COPTF = -O6 -fomit-frame-pointer -funroll-loops 
CPROF = -O2 -g -pg -fno-inline


all:	visu visu.prof

visu :	visu.c
	$(CC) $(CFLAGS) $(COPTF) -o $@ $^
	strip $@

visu.prof :	visu.c
	$(CC) $(CFLAGS) $(CPROF) -o $@ $^

clean:
	rm visu visu.prof
	rm -f *.orig

