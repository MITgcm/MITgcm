/*************************************************   -*- mode: C -*-
*/

/*  Decode and write files */

#include "FC_NAMEMANGLE.h"

#include <stdio.h>

#include "EMBEDDED_FILES.h"

/*   NOTE: output to stdout has been removed below since it is not a
 *   good idea to mix C stdio with Fortran stdio.
 */

void FC_NAMEMANGLE(embdec) ()
{
    FILE * fout;
    int ii, jj;

    for (ii=0; ii<n_flist; ii++) {
	/* printf("  decoding:  \"%s\"\n", flist[ii].name); */

	fout = fopen(flist[ii].name, "w");
	if (!fout) {
	    /*
	    printf("    WARNING:  cannot fopen() \"%s\" for writing\n",
		   flist[ii].name);
	    */
	    continue;
	}

	for (jj=0; jj<flist[ii].ndat; jj++) {
	    fprintf(fout,"%c",(unsigned char)(flist[ii].dat[jj]) );
	}
	fclose(fout);
    }
}

