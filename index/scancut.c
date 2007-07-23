/*
** Copyright (C) 2007 Herbert Poetzl
*/

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linux/errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

#define VERSION "V0.1"

static	char *cmd_name;


long long int dnatoll(const char *str, char **eptr, int len)
{
	long long int val = 0;
	
	while (*str && len--) {
	    switch (*str) {
	    case 'I':
	    case 'F':
		val = val << 1;
		break;
	    case 'C':
		val = (val << 1) + 1;
		break;
	    case 'P':
	    default:
	    	goto ret;
	    }
	    str++;
	}
ret:
	if (eptr)
	    *eptr = (char *)str;
	return val;
}

void ltodna(char *buf, unsigned long long int val)
{
	while (val) {
	     *buf++ = (val & 1) ? 'C' : 'I';
	     val >>= 1;
	}
	*buf++ = 'P';
	*buf = '\0';
}


long long int argtoll(const char *str, char **eptr)
{
	int bit, inv = 0, nbo = 0;
	long long int val = 0;

	if (!str)
	    return -1;
	
	switch (*str) {
	case '~':
	case '!':
	    inv = 1;	/* invert */
	    str++;
	default:
	    break;
	}

	while (*str) {
	    switch (*str) {
	    case '^':
		bit = strtol(str+1, eptr, 0);
		val ^= (1LL << bit);
		break;
	    case '&':
		val &= argtoll(str+1, eptr);
		break;
	    case '|':
		val |= argtoll(str+1, eptr);
		break;
	    case '+':
		val += argtoll(str+1, eptr);
		break;
	    case '-':
		val -= argtoll(str+1, eptr);
		break;
	    case '.':
		val = (val << 8) | (strtol(str+1, eptr, 0) & 0xff);
		nbo++;
		break;
	    case 'I':
	    case 'C':
	    case 'F':
	    case 'P':
		val = dnatoll(str, eptr, -1);
		break;
	    default:
		val = strtol(str, eptr, 0);
		break;
	    }
	    if (*eptr == str)
	    	break;
	    str = *eptr;
	}
	return (inv)?~(val):(val);
}


static unsigned long long opt_encode;

static char * opt_sequence = "IFPICFPPCFFPP";

static unsigned long long opt_offset = 0;
static unsigned long long opt_length = 24;

static char opt_decode = 0;

static int handle = 0;
static char *data;

int	main(int argc, char *argv[])
{
	extern int optind;
	extern char *optarg;
	int c, errflg = 0;
	char *str, *eptr, buffer[80];
	unsigned long long number = 8475;
	off_t flen, slen, location;
	
	cmd_name = argv[0];
	while ((c = getopt(argc, argv, "he:L:NS:O:")) != EOF) {
	    switch (c) {
	    case 'h':
		fprintf(stderr,
		    "This is %s " VERSION "\n"
		    "options are:\n"
		    "-h        print this help message\n"
		    "-e <num>  encode number\n"
		    "-L <cmd>  sequence length [%lld]\n"
		    "-N        decode number\n"
		    "-S <str>  base sequence [%s]\n"
		    "-O <num>  offset from base [%lld]\n"
		    , cmd_name 
		    , opt_length, opt_sequence, opt_offset);
		exit(0);
		break;
	    case 'e':
		opt_encode = argtoll(optarg, &eptr);
		break;
	    case 'S':
		opt_sequence = optarg;
		break;
	    case 'O':
		opt_offset = argtoll(optarg, &eptr);
		break;
	    case 'L':
		opt_length = argtoll(optarg, &eptr);
		break;
	    case 'N':
		opt_decode = 1;
		break;
	    case '?':
	    default:
		errflg++;
		break;
	    }
	}
	    
	if (optind == argc) {
	    fprintf(stderr, "missing argument.\n");
	    exit(1);
	} else {
	    handle = open(argv[optind], O_RDONLY);
	    if (handle == -1)
	    	perror("open");
	}

	printf("offset: 0x%016llX (%llu)\n", opt_offset, opt_offset);
	printf("length: 0x%016llX (%llu)\n", opt_length, opt_length);
	
	
	flen = lseek(handle, 0, SEEK_END);
	data = mmap(0, flen, PROT_READ, MAP_PRIVATE, handle, 0);
	if (data == (void *)-1)
	    perror("mmap");
	
	slen = strlen(opt_sequence);
	printf("memmem(»%s«,%ld,%p,%ld)\n",
		opt_sequence, slen, data, flen);
		
	str = memmem(data, flen, opt_sequence, slen);
	location = str - data;

	if (opt_offset > flen - location) {
	    opt_offset = flen - location;
	    printf("offset too far, adjusted: %lld\n", 
		opt_offset);
	}

	str += opt_offset;
	location += opt_offset;
	
	if (opt_length > flen - location) {
	    opt_length = flen - location;
	    printf("length too large, adjusted: %lld\n", 
		opt_length);
	}

	printf("match: »%s« »%.*s«\n",
		opt_sequence, (int)opt_length, str);
		
	number = dnatoll(str, NULL, (int)opt_length);
	ltodna(buffer, number);
	printf("number: 0x%016llX (%llu) %s\n",
		number, number, buffer);
	exit(0);
}
