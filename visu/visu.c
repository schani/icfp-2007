/*
** Copyright (C) 2007 Herbert Poetzl
*/

#include <stdio.h>
#include <stdlib.h>
#include <linux/errno.h>
#include <unistd.h>

#include <SDL/SDL.h>

#define SDL

#include "visu.h"
#include "target.h"



#define VERSION "V0.2"


static	char *cmd_name;


#ifdef	SDL
static SDL_Surface *screen;
static Uint8 *raw;
#endif




static	struct _bucket bucket;

static	unsigned width = 600;
static	unsigned height = 600;

static	struct _pos pos = {0, 0};
static	struct _pos mark = {0, 0};

static	unsigned dir = DIR_E;


static	struct _bitmap bitmaps[10];
static	unsigned layer = 0;

static	struct _bitmap risk;




static inline
void	recalc_col(struct _bucket *b)
{
	unsigned N = b->N;
	unsigned rc = N ? 255*b->R/N : 0;
	unsigned gc = N ? 255*b->G/N : 0;
	unsigned bc = N ? 255*b->B/N : 0;
	
	unsigned M = b->M;
	unsigned ac = M ? 255*b->A/M : 255;
	
	unsigned col = COL(rc*ac/255, gc*ac/255, bc*ac/255, ac);

	b->col = col;
}


static inline 
void	do_move(unsigned dir, struct _pos *pos)
{
	switch(dir) {
	case DIR_N: pos->Y = pos->Y ? pos->Y - 1 : 599; break;
	case DIR_E: pos->X = (pos->X + 1) % 600;	break;
	case DIR_S: pos->Y = (pos->Y + 1) % 600;	break;
	case DIR_W: pos->X = pos->X ? pos->X - 1 : 599;	break;
	}
}

static inline 
unsigned get_pixel(unsigned x, unsigned y)
{
	return bitmaps[layer].data[y][x];
}

static inline 
void	set_pixel(unsigned x, unsigned y, unsigned col)
{
	bitmaps[layer].data[y][x] = col;
}

static inline 
void	draw_line(unsigned x0, unsigned y0,
		  unsigned x1, unsigned y1, unsigned col)
{
	signed dx = x1 - x0;
	signed dy = y1 - y0;
	unsigned d = MAX(ABS(dx), ABS(dy));
	unsigned c = (dx*dy <= 0) ? 1 : 0;
	unsigned x = x0 * d + ((d - c)/2);
	unsigned y = y0 * d + ((d - c)/2);
	unsigned i;
	
	for (i = 0; i < d; i++) {
	    set_pixel(x/d, y/d, col);
	    x += dx;
	    y += dy;
	}
	set_pixel(x1, y1, col);
}


static	unsigned _fx, _fy;
static	unsigned _fi, _fc;

static inline 
void	_fill(void)
{
	unsigned old = get_pixel(_fx, _fy);
	
	if (_fi == old) {
	    set_pixel(_fx, _fy, _fc);
	    if (_fx > 0)   { _fx--; _fill(); _fx++; }
	    if (_fx < 599) { _fx++; _fill(); _fx--; }
	    if (_fy > 0)   { _fy--; _fill(); _fy++; }
	    if (_fy < 599) { _fy++; _fill(); _fy--; }
	}
}

static inline 
void	tryfill(unsigned x, unsigned y, unsigned col)
{
	unsigned old = get_pixel(x, y);
	
	if (col != old) {
	    _fx = x; _fy = y;
	    _fi = old; _fc = col;
	    _fill();
	}
}

static inline 
void	add_bitmap(void)
{
	if (layer < 9) {
	    layer++;
	    memset(bitmaps[layer].data[0],
		0, sizeof(struct _bitmap));
	}
}


static inline
unsigned _compose(unsigned c0, unsigned c1)
{
	unsigned r0 = RVAL(c0);
	unsigned g0 = GVAL(c0);
	unsigned b0 = BVAL(c0);
	unsigned a0 = AVAL(c0);
	
	unsigned r1 = RVAL(c1);
	unsigned g1 = GVAL(c1);
	unsigned b1 = BVAL(c1);
	unsigned a1 = AVAL(c1);
	
	register unsigned a0i = 255 - a0;
	
	return COL(r0 + (r1 * a0i / 255),
		   g0 + (g1 * a0i / 255),
		   b0 + (b1 * a0i / 255),
		   a0 + (a1 * a0i / 255));
}


static inline 
void	do_compose(void)
{
	if (layer > 0) {
	    unsigned *src = bitmaps[layer--].data[0];
	    unsigned *dst = bitmaps[layer].data[0];
	    unsigned cnt = 600*600;
	    
	    while (cnt--) {
		register unsigned cs = *src++;
		register unsigned cd = *dst;
		
		*dst++ = _compose(cs, cd);
	    }
	}
}


static inline
unsigned _clip(unsigned c0, unsigned c1)
{
	unsigned a0 = AVAL(c0);
	
	unsigned r1 = RVAL(c1);
	unsigned g1 = GVAL(c1);
	unsigned b1 = BVAL(c1);
	unsigned a1 = AVAL(c1);
	
	return COL(r1 * a0 / 255,
		   g1 * a0 / 255,
		   b1 * a0 / 255,
		   a1 * a0 / 255);
}

static inline 
void	do_clip(void)
{
	if (layer > 0) {
	    unsigned *src = bitmaps[layer--].data[0];
	    unsigned *dst = bitmaps[layer].data[0];
	    unsigned cnt = 600*600;
	    
	    while (cnt--) {
		unsigned cs = *src++;
		unsigned cd = *dst;
		
		*dst++ = _clip(cs, cd);
	    }
	}

}



static	void build_cmd(char cmd)
{
	switch(cmd) {
	case 'K': 					goto color;
	case 'R': bucket.R++;				goto color;
	case 'G': bucket.G++;				goto color;
	case 'B': bucket.B++;				goto color;
	case 'Y': bucket.R++; bucket.G++;		goto color;
	case 'M': bucket.R++; bucket.B++;		goto color;
	case 'C': bucket.G++; bucket.B++;		goto color;
	case 'W': bucket.R++; bucket.G++; bucket.B++;
	   color:
		bucket.N++; recalc_col(&bucket);
		break;

	case 'O': bucket.A++;
	case 'T': bucket.M++; 
		recalc_col(&bucket);
		break;

	case 'e': bucket.R = bucket.G = bucket.B = 0;
		  bucket.A = 0; 
		  bucket.N = bucket.M = 0;
		recalc_col(&bucket);
		break;

	case '^': do_move(dir, &pos);			break;
	case '<': dir = (dir - 1) & 3;			break;
	case '>': dir = (dir + 1) & 3;			break;
	case '=': mark = pos;				break;
	
	case '-': draw_line(mark.X, mark.Y,
			pos.X, pos.Y, bucket.col);	break;

	case '!': tryfill(pos.X, pos.Y, bucket.col);	break;
	case '+': add_bitmap();				break;
	case '*': do_compose();				break;
	case '&': do_clip();				break;
	
	default:					break;
	}
}




void	__color_test(void)
{
	static char *seqs[] = {
		"eOOT",
		"eCYK",
		"eOTY",
		"eTOOO"
		"WWWWWWWWWW"
		"MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"
		"RRRRRRR"
		"KKKKKKKKKKKKKKKKKK",
		NULL };
	
	char *seq;
	unsigned i = 0;
	
	while ((seq = seqs[i++])) {
	    while (*seq)
		build_cmd(*seq++);
	    printf("COL: ((%d,%d,%d),%d)\n",
		RVAL(bucket.col), GVAL(bucket.col),
		BVAL(bucket.col), AVAL(bucket.col));
	}
}



void	visualize(struct _bitmap *bm)
{
#ifdef	SDL
	SDL_LockSurface(screen);
#endif
	memcpy(raw, (void *)bm->data[0],
		sizeof(struct _bitmap));
#ifdef	SDL
	SDL_UnlockSurface(screen);
	SDL_UpdateRect(screen,0,0,0,0);
#endif
}


#define	CMP(a,b,l,e,g)			\
	(((a) == (b)) ? (e) :		\
	((a) < (b)) ? (l) : (g))


void	calc_risk(struct _bitmap *bm, struct _bitmap *result)
{
	unsigned row, col;
	unsigned *src, *dst, *tgt;

	src = bm->data[0];
	dst = result->data[0];
	tgt = target.data[0];
	
	for (row = 0; row < 600; row++) {
	    for (col = 0; col < 600; col++) {
	    	unsigned v0 = (*src++ | 0xFF000000);
	    	unsigned v1 = *tgt++;
		
		*dst++ = COL(
		    CMP(RVAL(v0), RVAL(v1), 0x80, 0xFF, 0x00),
		    CMP(GVAL(v0), GVAL(v1), 0x80, 0xFF, 0x00),
		    CMP(BVAL(v0), BVAL(v1), 0x80, 0xFF, 0x00),
		    0x00);
	    }
	}
}


void	write_ppm(struct _bitmap *bm, FILE *fp)
{
	unsigned row, col;
	fprintf(fp, "P3\n# CREATOR %s %s\n600 600\n255\n",
	    cmd_name, VERSION);
	for (row = 0; row < 600; row++) {
	    unsigned *ptr = bm->data[row];
	    
	    for (col = 0; col < 600; col++) {
	    	unsigned val = ptr[col];
		
	    	fprintf(fp, "%d %d %d\n", RVAL(val), GVAL(val), BVAL(val));
	    }
	}
}


static	char opt_compact = 0;
static	char opt_exitchar = 0;
static	char opt_interactive = 0;
static	char opt_step = 0;


static	unsigned opt_sleep = 0;

int	main(int argc, char *argv[])
{
	// int x, y;
	char line[MAXLINE];
	extern int optind;
	extern char *optarg;
	int c, errflg = 0;
	unsigned step = 1;
	
	cmd_name = argv[0];
	while ((c = getopt(argc, argv, "hs:n:CEI")) != EOF) {
	    switch (c) {
	    case 'h':
		fprintf(stderr,
		    "This is %s " VERSION "\n"
		    "options are:\n"
		    "-h        print this help message\n"
		    "-s <sec>  sleep <sec> seconds\n"
		    "-n <num>  visualize every <num> steps\n"
		    "-C        compact character sequence\n"
		    "-E        enable exit char '.'\n"
		    "-I        interactive\n"
		    , cmd_name);
		exit(0);
		break;
	    case 's':
		opt_sleep = atoi(optarg);
		break;
	    case 'n':
		opt_step = atoi(optarg);
		break;
	    case 'C':
		opt_compact = 1;
		break;
	    case 'E':
		opt_exitchar = 1;
		break;
	    case 'I':
		opt_interactive = 1;
		break;
	    case '?':
	    default:
		errflg++;
		break;
	    }
	}

	// width = atoi(argv[1]);
	// height = atoi(argv[2]);

#ifdef	SDL
	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
	    printf("Unable to initialize SDL: %s\n", SDL_GetError());
	    exit(1);
	}
	atexit(SDL_Quit);
	screen = SDL_SetVideoMode(width, height, 32, 0);
	if (screen == NULL) {
	    printf("Unable to set video mode: %s\n", SDL_GetError());
	    exit(1);
	}
	raw = (Uint8 *)screen->pixels;
#endif

/*
	__color_test();


	for (x = 0; x < 255; x++) {
	    for (y = 0; y < 100; y++) {
		set_pixel(x, y, 0xFFFF00);
	    }
	}
	for (x = 200; x < 300; x++) {
	    for (y = 100; y < 300; y++) {
		set_pixel(x, y, 0xFFFF00);
	    }
	}
	
*/

	while (1) {
	    int c;
	    
	    if (opt_compact) {
		c = fgetc(stdin);
		if (c == EOF)
		    break;
	    } else {
		if (!fgets(line, MAXLINE, stdin))
		    break;
		c = line[0];
	    }
	    if (opt_exitchar && (c == '.'))
	    	break;

	    build_cmd(c);
	
	    if (opt_interactive) {
		if (!opt_step || !(step % opt_step)) {
		    visualize(&bitmaps[layer]);
		    fputc('.', stderr);
	        }
	    }
	    step++;
	}
	
	write_ppm(&bitmaps[0], stdout);
	visualize(&bitmaps[0]);
	
	sleep(5);
	calc_risk(&bitmaps[0], &risk);
	visualize(&risk);
	
	sleep(5);
	if (opt_sleep)
	    sleep(opt_sleep);
	
	exit(0);
}
