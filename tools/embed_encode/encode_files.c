/*************************************************   -*- mode: C -*-
**
** $Header: /u/gcmpack/MITgcm/tools/embed_encode/encode_files.c,v 1.1 2006/01/11 04:50:30 edhill Exp $
** $Name:  $
**
*/

/*  Encode one or more files for later writing */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

int main(int argc, char** argv)
{
    FILE * fout;
    FILE * fin;
    int ii;
    int nbytes;
    struct stat astat;
    unsigned char c;
    unsigned char buf[1024];

    /*
    fout = fopen("t8", "w");
    for (ii=0; ii<256; ii++) {
	c = (unsigned char)ii;
	fprintf(fout,"%c",c);
    }
    fclose(fout);
    */

    if (argc < 3) {
	printf("ERROR:  usage is: "
	       "\"%s OUTPUT_FILE INPUT_FILE [...INPUT_FILES...]\"\n",
	       argv[0]);
	return 1;
    }

    fout = fopen(argv[1], "w");
    if (! fout) {
	printf("ERROR: cannot open \"%s\" for output\n", argv[1]);
	return 1;
    }
    fprintf(fout,"struct embeded_file {\n    char * name;\n"
            "    int ndat;\n    char * dat;\n};\n");
    fprintf(fout,"const int n_flist = %d;\n", argc-2);
    fprintf(fout,"struct embeded_file flist[] = {\n");
    
    for (ii=2; ii<argc; ii++) {
	printf("  encoding:  \"%s\"\n",argv[ii]);
	if (! stat(argv[ii], &astat)) {
	    nbytes = (int)(astat.st_size);
	}
	else {
	    printf("    ERROR:  cannot stat() \"%s\"\n",argv[ii]);
	    return 3;
	}

	fin = fopen(argv[ii], "r");
	if (!fin) {
	    printf("    ERROR:  cannot fopen() \"%s\"\n",argv[ii]);
	    return 4;
	}

	fprintf(fout,"  %s{ \"%s\", %d,", ((ii==2) ? " " : ","),
		argv[ii], nbytes);
	while(fin) {
	    size_t ir;
	    size_t nread;

	    /* fgets(buf, 255, fin); */
	    nread = fread(buf, sizeof(char), 200, fin);
	    if (nread < 1)
		break;
	    fprintf(fout,"\n\"");
	    for (ir=0; ir<nread; ir++) {
		fprintf(fout,"\\x%02x",buf[ir]);
		/*
		if (isalnum(buf[ir]))
		    fprintf(fout,"%c",buf[ir]);
		else
		    fprintf(fout,"\\x%02x",buf[ir]);
		*/
	    }
	    fprintf(fout,"\"");
	}
	fclose(fin);
	fprintf(fout,"  }\n");
    }
    fprintf(fout,"  };\n");
    fclose(fout);

    return 0;
}

