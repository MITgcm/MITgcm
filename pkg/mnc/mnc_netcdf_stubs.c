/*************************************************  -*- mode: C -*-
 * $Header: /u/gcmpack/MITgcm/pkg/mnc/Attic/mnc_netcdf_stubs.c,v 1.1 2004/07/26 22:13:51 edhill Exp $
 * $Name:  $
 */

/*
 *  PREF   = { "", "_", "__" }
 *  SUFF   = { "", "_", "__" }
 *  FTYPE  = { "DOUBLE", "REAL",  "INT" }
 *  CTYPE  = { "double", "float", "int" }
 *
 */

#include <stddef.h> /* size_t, ptrdiff_t */
#include <errno.h>  /* netcdf functions sometimes return system errors */


#define MNCS_PUT_VARA_( PREF, SUFF, FTYPE, CTYPE ) \
    int PREF ## MNCS_PUT_VARA_ ## FTYPE ## SUFF ( int ncid, int varid, const size_t *startp, \
				                  const size_t *countp, const CTYPE *op ) { \
    return PREF ## nc_put_vara_ ## CTYPE ## SUFF ( ncid, varid, startp, countp, op ); }



MNCS_PUT_VARA_(   ,   , DOUBLE, double )
MNCS_PUT_VARA_(   ,   , REAL, float )
MNCS_PUT_VARA_(   ,   , INT, int )
