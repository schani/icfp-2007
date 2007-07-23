
CFLAGS = -Wall
LFLAGS = 
COPTF = -O6 -fomit-frame-pointer -funroll-loops 
CPROF = -O2 -g -pg -fno-inline


all:	scancut topoly

scancut : scancut.c
	$(CC) $(CFLAGS) $(COPTF) $(LFLAGS) -o $@ $^
	strip $@

topoly : topoly.c
	$(CC) $(CFLAGS) $(COPTF) $(LFLAGS) -o $@ $^
	strip $@

clean:
	rm -f scancut
	rm -f *.orig

