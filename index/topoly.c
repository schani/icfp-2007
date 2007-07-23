/*
** Copyright (C) 2007 Herbert Poetzl
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linux/errno.h>
#include <unistd.h>

#define VERSION "V0.1"

static	char *cmd_name;



void ltodna(char *buf, unsigned long long int val, int len)
{
	while (len--) {
	     *buf++ = (val & 1) ? 'C' : 'I';
	     val >>= 1;
	}
	*buf++ = 'P';
	*buf = '\0';
}


int	main(int argc, char *argv[])
{
	extern int optind;
	extern char *optarg;
	int c, errflg = 0, first = 1;
	char line[80], buffer[80];
	
	cmd_name = argv[0];
	while ((c = getopt(argc, argv, "h")) != EOF) {
	    switch (c) {
	    case 'h':
		fprintf(stderr,
		    "This is %s " VERSION "\n"
		    "options are:\n"
		    "-h        print this help message\n"
		    , cmd_name);
		exit(0);
		break;
	    case '?':
	    default:
		errflg++;
		break;
	    }
	}
	    
	while (fgets(line, sizeof(line), stdin)) {
	    signed val;
	    char ch, n;
	    
	    n = sscanf(line, "%c %d", &ch, &val);
	
	    ltodna(buffer, val, 11);
	    // printf("%c %5d : %s\n", ch, val, buffer);
	    if (!first) {
	        if (ch == '#') printf("\n");
	    } else 
	    	first = 0;
	    
	    printf("%s", buffer);
	}
	
	exit(0);
}
