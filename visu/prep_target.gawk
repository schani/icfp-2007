
BEGIN	{ 
	  printf "\n"; 
	  printf "#include \"visu.h\"\n"; 
	  printf "#include \"target.h\"\n"; 
	  printf "\n"; 
	  printf "struct _bitmap target = { .data = ";
	}

	{ L++ }

(L==4)	{ printf "{{\n\t"; }
	
(L>4)	{ if (!((L-2) % 3))
		printf "0xFF";
	  printf "%02X", $1;
	  if (!((L-4) % 3))
	  	printf ", ";
	  if (!((L-4) % (600*3)) && ((L-4) != 600*600*3)) 
	  	printf "\n}, {\n\t";
	  else if (!((L-4) % 12)) 
	  	printf "\n\t";
	}

(L<1)	{
	  if (!((L-4) % (600*3))) 
	  	printf "\n}, {\n\t";
	}

END	{ 
	  printf "}}\n};\n"; 
	}
