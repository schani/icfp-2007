#!/bin/bash

cat $1 | \
    tr -s '\n\t ' ' ' | tr -s '</>"' '\n' | \
    sed -n '/^M/ {s/C/\nC/; s/,/:/g; s/\([0-9]\)[ ]/\1\nC /g; p }' | \
    tr ':' ' ' | sed 's/[.][0-9]*//g' | \
    gawk '
	
	(NF<3)	{ next }
	
	/^M/	{ C=0; M++; N=0;
		  /* printf "new path @ [%d,%d]\n",$2,$3; */
		  X[M,N] = $2; Y[M,N] = $3; N++;
		}
		  
	/^C/	{ C++; 
		  if ((C%3) == 1) {
		      # printf (" (%d,%d) -> ",$2,$3);
		      X[M,N] = $2; Y[M,N] = $3; N++;
		  } else if ((C%3) == 0) {
		      # printf ("(%d,%d)\n",$2,$3); */
		      X[M,N] = $2; Y[M,N] = $3; V[M] = N;
		  }
		}
		
	END	{
		  for (m=1; m<=M; m++) {
		    printf("(");
		    for (n=1; n<=V[m]; n++) {
		      if (n==1) printf("(%d . %d)", X[m,n], Y[m,n]);
		      else printf("(%d . %d)", X[m,n]-X[m,n-1], Y[m,n]-Y[m,n-1]);
		      # printf(" %d: (%d,%d) ", n, X[m,n], Y[m,n]);
		      # printf(" [%d,%d]\n", X[m,n]-X[m,n-1], Y[m,n]-Y[m,n-1]);
		    }
		    printf("(%d . %d))\n", X[m,n-1]-X[m,0], Y[m,n-1]-Y[m,0]);
		  }
		}
    '

