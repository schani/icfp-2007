
#define	WIDTH	600
#define	HEIGHT	600

#define	MAXLINE	4096


#define	COL_K	0x000000
#define	COL_R	0xFF0000
#define	COL_G	0x00FF00
#define	COL_Y	0xFFFF00
#define	COL_B	0x0000FF
#define	COL_M	0xFF00FF
#define	COL_C	0x00FFFF
#define	COL_W	0xFFFFFF


#define	ALPHA_T	0x00
#define	ALPHA_O	0xFF


#define	MAX(a, b)	(((a) > (b)) ? (a) : (b))
#define	MIN(a, b)	(((a) <= (b)) ? (a) : (b))

#define	ABS(a)		(((a) >= 0) ? (a) : -(a))


#define	COL(r,g,b,a)	(((a) << 24) | ((r) << 16) | ((g) << 8) | (b))

#define	RVAL(c)		(((c) >> 16) & 0xFF)
#define	GVAL(c)		(((c) >> 8) & 0xFF)
#define	BVAL(c)		(((c) >> 0) & 0xFF)
#define	AVAL(c)		(((c) >> 24) & 0xFF)


enum {
	DIR_N = 0,
	DIR_E,
	DIR_S,
	DIR_W
};

struct _bucket {
	unsigned R;
	unsigned G;
	unsigned B;
	unsigned N;
	
	unsigned A;
	unsigned M;
	
	unsigned col;
	unsigned dirty;
};

struct _pos {
	unsigned X;
	unsigned Y;
};


struct _bitmap {
	unsigned data[WIDTH][HEIGHT];
};

struct _state {
	struct _bucket bucket;

	struct _pos pos;
	struct _pos mark;

	unsigned dir;

	struct _bitmap bitmaps[10];
	unsigned layer;
};
