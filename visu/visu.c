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



#define VERSION "V0.5"


static	char *cmd_name;


#ifdef	SDL
static SDL_Surface *screen;
static SDL_Surface *bms[10];
static Uint8 *raw;
#endif


static	struct _state master;	

static	struct _bitmap risk;

static	FILE *fdata = NULL;

static	unsigned index_cur = 0;
static	unsigned index_max = 1 << 30;

static	char opt_compact = 0;
static	char opt_exitchar = 0;
static	char opt_interactive = 0;
static	char opt_writeppm = 0;
static	char opt_writerisk = 0;
static	char opt_stepsize = 0;
static	char opt_novisual = 0;
static	char opt_showrisk = 0;
static	char opt_blackwhite = 0;

static	unsigned opt_sleep = 0;





void	init_state(struct _state *state)
{
	memset(state, 0, sizeof(struct _state));
	
	state->bucket.R = 0; state->bucket.G = 0;
	state->bucket.B = 0; state->bucket.N = 0;
	state->bucket.A = 0; state->bucket.M = 0;
	state->bucket.col = 0;
	
	state->pos.X = 0; state->pos.Y = 0;
	state->mark.X = 0; state->mark.Y = 0;
	state->dir = DIR_E;
	state->layer = 0;
}


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
unsigned get_pixel(struct _state *s, unsigned x, unsigned y)
{
	return s->bitmaps[s->layer].data[y][x];
}

static inline 
void	set_pixel(struct _state *s, unsigned x, unsigned y, unsigned col)
{
	s->bitmaps[s->layer].data[y][x] = col;
}

static inline 
void	draw_line(struct _state *s)
{
	unsigned x0 = s->mark.X;
	unsigned y0 = s->mark.Y;
	unsigned x1 = s->pos.X;
	unsigned y1 = s->pos.Y;
	
	signed dx = x1 - x0;
	signed dy = y1 - y0;
	unsigned d = MAX(ABS(dx), ABS(dy));
	unsigned c = (dx*dy <= 0) ? 1 : 0;
	unsigned x = x0 * d + ((d - c)/2);
	unsigned y = y0 * d + ((d - c)/2);
	unsigned i, col;
	
	if (s->bucket.dirty)
	    recalc_col(&s->bucket);
	
	col = s->bucket.col;
	for (i = 0; i < d; i++) {
	    set_pixel(s, x/d, y/d, col);
	    x += dx;
	    y += dy;
	}
	set_pixel(s, x1, y1, col);
}


static	unsigned _fx, _fy;
static	unsigned _fi, _fc;

static inline 
void	_fill(struct _state *s)
{
	unsigned old = get_pixel(s, _fx, _fy);
	
	if (_fi == old) {
	    set_pixel(s, _fx, _fy, _fc);
	    if (_fx > 0)   { _fx--; _fill(s); _fx++; }
	    if (_fx < 599) { _fx++; _fill(s); _fx--; }
	    if (_fy > 0)   { _fy--; _fill(s); _fy++; }
	    if (_fy < 599) { _fy++; _fill(s); _fy--; }
	}
}

static inline 
void	tryfill(struct _state *s)
{
	unsigned x = s->pos.X;
	unsigned y = s->pos.Y;
	unsigned old = get_pixel(s, x, y);
	unsigned col;

	if (s->bucket.dirty)
	    recalc_col(&s->bucket);
	
	col = s->bucket.col;
	if (col != old) {
	    _fx = x; _fy = y;
	    _fi = old; _fc = col;
	    _fill(s);
	}
}

static inline 
void	add_bitmap(struct _state *s)
{
	if (s->layer < 9) {
	    s->layer++;
	    memset(s->bitmaps[s->layer].data[0],
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
void	do_compose(struct _state *s)
{
	if (s->layer > 0) {
	    unsigned *src = s->bitmaps[s->layer--].data[0];
	    unsigned *dst = s->bitmaps[s->layer].data[0];
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
void	do_clip(struct _state *s)
{
	if (s->layer > 0) {
	    unsigned *src = s->bitmaps[s->layer--].data[0];
	    unsigned *dst = s->bitmaps[s->layer].data[0];
	    unsigned cnt = 600*600;
	    
	    while (cnt--) {
		unsigned cs = *src++;
		unsigned cd = *dst;
		
		*dst++ = _clip(cs, cd);
	    }
	}

}


static	void build_cmd(struct _state *s, char cmd)
{
	struct _bucket *b = &s->bucket;

	switch(cmd) {
	case 'K': 					goto color;
	case 'R': b->R++;				goto color;
	case 'G': b->G++;				goto color;
	case 'B': b->B++;				goto color;
	case 'Y': b->R++; b->G++;			goto color;
	case 'M': b->R++; b->B++;			goto color;
	case 'C': b->G++; b->B++;			goto color;
	case 'W': b->R++; b->G++; b->B++;
	   color:
		b->N++; b->dirty = 1;
		break;

	case 'O': b->A++;
	case 'T': b->M++; 
		  b->dirty = 1;
		break;

	case 'e': b->R = b->G = b->B = 0;
		  b->A = 0; 
		  b->N = b->M = 0;
		  b->dirty = 1;
		break;

	case '^': do_move(s->dir, &s->pos);		break;
	case '<': s->dir = (s->dir - 1) & 3;		break;
	case '>': s->dir = (s->dir + 1) & 3;		break;
	case '=': s->mark = s->pos;			break;
	
	case '-': draw_line(s);				break;

	case '!': tryfill(s);				break;

	case '+': add_bitmap(s);			break;
	case '*': do_compose(s);			break;
	case '&': do_clip(s);				break;
	
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
		build_cmd(&master, *seq++);
	    printf("COL: ((%d,%d,%d),%d)\n",
		RVAL(master.bucket.col), GVAL(master.bucket.col),
		BVAL(master.bucket.col), AVAL(master.bucket.col));
	}
}




#define	CMP(a,b,l,e,g)			\
	(((a) == (b)) ? (e) :		\
	((a) < (b)) ? (l) : (g))


unsigned calc_risk(struct _bitmap *bm, struct _bitmap *result)
{
	unsigned row, col;
	unsigned *src, *dst, *tgt;
	unsigned val = 0;

	src = bm->data[0];
	dst = result->data[0];
	tgt = target.data[0];
	
	for (row = 0; row < 600; row++) {
	    for (col = 0; col < 600; col++) {
		unsigned v0 = (*src++ | 0xFF000000);
		unsigned v1 = *tgt++;
		
		if (v0 != v1) val++;
		*dst = COL(
		    CMP(RVAL(v0), RVAL(v1), 0x80, 0x00, 0xFF),
		    CMP(GVAL(v0), GVAL(v1), 0x80, 0x00, 0xFF),
		    CMP(BVAL(v0), BVAL(v1), 0x80, 0x00, 0xFF),
		    0x00);
		if (opt_blackwhite && *dst) *dst = 0xFFFFFF;
		dst++;
	    }
	}
	return val;
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



#define UI_QUIT     1


static	char ui_stop = 0;

static	unsigned ui_delay = 0;
static	unsigned ui_stepsize = 10;
static	unsigned ui_jumpto = 0;
static	unsigned ui_reload = 0;


#define	UI_OUTPUT(v, f, a...)	\
	printf("%s: " f "\n", v, a);


void	black_white(void *data)
{
	unsigned *ptr = (unsigned *)data;
	unsigned cnt = 600*600;
	
	while (cnt--) {
	    *ptr = *ptr ? 0xFFFFFF : 0x0;
	    ptr++;
	}
}

void	visualize(struct _bitmap *bm)
{
	if (opt_novisual)
	    return;
#ifdef	SDL

#if 1
	SDL_LockSurface(screen);
	memcpy(raw, (void *)bm->data[0],
		sizeof(struct _bitmap));

	if (opt_blackwhite)
	    black_white(raw);

	SDL_UnlockSurface(screen);
#else
	if (!(index_cur % 1000))
	    SDL_BlitSurface(bms[0], &rect, screen, &rect);
#endif

	// SDL_UpdateRect(screen,0,0,0,0);
	SDL_Flip(screen);
#endif
	UI_OUTPUT("showing", "index %8d", index_cur);
}


int	user_input(void)
{
	SDL_Event event;
	int quit = 0;

	if (ui_stop)
	    SDL_WaitEvent(&event);
	else 
	    SDL_PollEvent(&event);

	switch (event.type) {
	case SDL_KEYDOWN:
	    switch (event.key.keysym.sym) {
	    case SDLK_LEFT:
		if (ui_jumpto) break;
		ui_jumpto = CLAMP(index_cur - 11, 0, index_max);
		UI_OUTPUT("returning", "to index %d", ui_jumpto);
		break;
	    case SDLK_RIGHT:
		if (ui_jumpto) break;
		ui_jumpto = CLAMP(index_cur + 9, 0, index_max);
		UI_OUTPUT("advancing", "to index %d", ui_jumpto);
		break;
	    case SDLK_DOWN:
		if (ui_jumpto) break;
		ui_jumpto = CLAMP(index_cur - 101, 0, index_max);
		UI_OUTPUT("returning", "to index %d", ui_jumpto);
		break;
	    case SDLK_UP:
		if (ui_jumpto) break;
		ui_jumpto = CLAMP(index_cur + 99, 0, index_max);
		UI_OUTPUT("advancing", "to index %d", ui_jumpto);
		break;
	    case SDLK_PAGEDOWN:
		if (ui_jumpto) break;
		ui_jumpto = CLAMP(index_cur - 1001, 0, index_max);
		UI_OUTPUT("returning", "to index %d", ui_jumpto);
		break;
	    case SDLK_PAGEUP:
		if (ui_jumpto) break;
		ui_jumpto = CLAMP(index_cur + 999, 0, index_max);
		UI_OUTPUT("advancing", "to index %d", ui_jumpto);
		break;
	    case SDLK_END:
		if (ui_jumpto) break;
		ui_jumpto = index_max;
		UI_OUTPUT("advancing", "to index %d", ui_jumpto);
		break;
	    case SDLK_HOME:
		if (ui_jumpto) break;
		ui_jumpto = 1;
		UI_OUTPUT("returning", "to index %d", ui_jumpto);
		break;

	    case SDLK_PLUS:
		ui_stepsize++;
		break;
	    case SDLK_MINUS:
		if (ui_stepsize)
		    ui_stepsize--;
		break;
	    case SDLK_LESS:
		ui_delay++;
		break;
	    case SDLK_GREATER:
		if (ui_delay)
		    ui_delay--;
		break;
	    
	    
	    case SDLK_SPACE:
		UI_OUTPUT("paused", "at index %8d", index_cur);
		ui_stop = 1;
		break;
		
	    case 'r':
		UI_OUTPUT("reload", "advancing to %d", index_cur);
		ui_jumpto = index_cur;
		ui_reload = 1;
		break;
	    case 'p':
		UI_OUTPUT("resume", "from index %8d", index_cur);
		ui_stop = 0;
		break;

	    case 'i':
		break;
	    case 'q':
		quit = 1;
		break;

	    default :
		break;
	    }
	    break;

	case SDL_KEYUP :
	    switch (event.key.keysym.sym) {
	    case SDLK_RIGHT:
	    case SDLK_UP:
	    case SDLK_PAGEUP:
	    case SDLK_END:
		// UI_OUTPUT("advancing", "to index %d", ui_jumpto);
		break;

	    case SDLK_LEFT:
	    case SDLK_DOWN:
	    case SDLK_PAGEDOWN:
	    case SDLK_HOME:
		// UI_OUTPUT("returning", "to index %d", ui_jumpto);
		break;

	    case SDLK_PLUS:
	    case SDLK_MINUS:
		UI_OUTPUT("stepsize", "set to %d", ui_stepsize);
		break;

	    case SDLK_LESS:
	    case SDLK_GREATER:
		UI_OUTPUT("delay", "set to %d", ui_delay);
		break;

	    case 'r':
		break;
	    case 'p':
		break;
	    case 'i':
		break;
	    default :
		break;
	    }
	    break;


	case SDL_QUIT :
	/*  printf("quit received ....\n");
	    quit = 1; */
	    break;
	}
	return quit;
}


int	input_cmd(FILE *fp)
{
	char line[MAXLINE];
	int c;

	if (opt_compact) {
	    c = fgetc(fp);
	    if (c == EOF)
		return c;
	} else {
	    if (!fgets(line, MAXLINE, fp))
		return EOF;
	    c = line[0];
	}
	return c;
}


void	file_rewind(FILE *fp)
{
	init_state(&master);
	fseek(fp, 0, SEEK_SET);
	index_cur = 0;
}


int	main(int argc, char *argv[])
{
	extern int optind;
	extern char *optarg;
	int c, errflg = 0;
	
	cmd_name = argv[0];
	while ((c = getopt(argc, argv, "hbs:n:qrCEIRW")) != EOF) {
	    switch (c) {
	    case 'h':
		fprintf(stderr,
		    "This is %s " VERSION "\n"
		    "options are:\n"
		    "-h        print this help message\n"
		    "-b        black and white mode\n"
		    "-s <sec>  sleep <sec> seconds\n"
		    "-n <num>  visualize every <num> steps\n"
		    "-q        disable visualization\n"
		    "-r        show risk information\n"
		    "-C        compact character sequence\n"
		    "-E        enable exit char '.'\n"
		    "-I        interactive input\n"
		    "-R        write risk map to stdout\n"
		    "-W        write bitmap to stdout\n"
		    , cmd_name);
		exit(0);
		break;
	    case 'b':
		opt_blackwhite = 1;
		break;
	    case 's':
		opt_sleep = atoi(optarg);
		break;
	    case 'n':
		opt_stepsize = atoi(optarg);
		break;
	    case 'q':
		opt_novisual = 1;
		break;
	    case 'r':
		opt_showrisk = 1;
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
	    case 'R':
		opt_writerisk = 1;
		break;
	    case 'W':
		opt_writeppm = 1;
		break;
	    case '?':
	    default:
		errflg++;
		break;
	    }
	}

	if (!opt_interactive) {
	    if (optind == argc) {
		fprintf(stderr, "missing argument.\n");
		exit(1);
	    } else {
		fdata = fopen(argv[optind], "r");
	    }
	}

#ifdef	SDL
	if (opt_novisual)
	    goto skip_sdl;
	
	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
	    printf("Unable to initialize SDL: %s\n", SDL_GetError());
	    exit(1);
	}
	atexit(SDL_Quit);
	screen = SDL_SetVideoMode(WIDTH, HEIGHT, 32,
	    SDL_ASYNCBLIT | SDL_DOUBLEBUF);
	if (screen == NULL) {
	    printf("Unable to set video mode: %s\n", SDL_GetError());
	    exit(1);
	}
	raw = (Uint8 *)screen->pixels;

	{ 
	    int i;
	    
	    for (i = 0; i < 10; i++) {
		bms[i] = SDL_CreateRGBSurfaceFrom(
		    &master.bitmaps[i].data[0],
		    WIDTH, HEIGHT, 32, WIDTH*4,
		    0xFF0000, 0xFF00, 0xFF, 0xFF000000);
	    }
	}
	SDL_EnableKeyRepeat(500, 100);
skip_sdl:
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

	init_state(&master);

	if (opt_interactive) {
	    while (1) {
		int c = input_cmd(stdin);

		if (c == EOF)
		    break;
		if (opt_exitchar && (c == '.'))
		    break;

		build_cmd(&master, c);

		if (!opt_stepsize || !(index_cur % opt_stepsize)) {
		    visualize(&master.bitmaps[master.layer]);
		    fputc('.', stderr);
		}
		index_cur++;
	    }
	    exit(0);
	}
	
	
	// visualize(&risk);
	/* 
	sleep(5);
	calc_risk(&master.bitmaps[0], &risk);
	
	sleep(5);
	*/
	
	if (opt_stepsize)
	    ui_stepsize = opt_stepsize;
	
	while (1) {
	    if (!ui_stop || ui_jumpto) {
		int c = input_cmd(fdata);
		
		build_cmd(&master, c);
		if (c == EOF) {
		    ui_stop = 1;
		    index_max = index_cur;
		    visualize(&master.bitmaps[master.layer]);
		    ui_jumpto = 0;
		} else {
		    if (ui_jumpto) {
			if (ui_jumpto < index_cur) {
			    file_rewind(fdata);
			    index_cur = 0;
			}
			if (index_cur++ < ui_jumpto)
			    continue;
			visualize(&master.bitmaps[master.layer]);
			ui_jumpto = 0;
			continue;
		    } 
		    if (!ui_stepsize  || !(index_cur % ui_stepsize)) {
			visualize(&master.bitmaps[master.layer]);
			if (ui_delay)
			    usleep(ui_delay);
		    }
		    index_cur++;
		}
	    }
	    if (opt_novisual && ui_stop)
		break;
	    else if (user_input())
		exit(1);
	};

	visualize(&master.bitmaps[0]);
	if (opt_writeppm)
	    write_ppm(&master.bitmaps[0], stdout);
	if (opt_writerisk) {
	    calc_risk(&master.bitmaps[0], &risk);
	    write_ppm(&risk, stdout);
	}
	if (opt_showrisk) {
	    unsigned val = calc_risk(&master.bitmaps[0], &risk);
	    fprintf(stdout, "%d\n", val*10);
	}


	if (opt_sleep)
	    sleep(opt_sleep);
	exit(0);
}
