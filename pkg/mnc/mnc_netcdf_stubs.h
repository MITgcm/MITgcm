/*************************************************  -*- mode: C -*-
 * $Header: /u/gcmpack/MITgcm/pkg/mnc/Attic/mnc_netcdf_stubs.h,v 1.1 2004/07/27 03:19:56 edhill Exp $
 * $Name:  $
 */

/*

Find all the NF_

a=`grep -i NF_ *.F *.template | sed -e 's|(| |g' | sed -e 's|,| |g' | sed -e 's|)| |g'` 
for i in $a ; do 
    echo $i | awk '(tolower(substr($0,0,3))=="nf_"){print tolower($0)}'
done | sort | uniq | sed -e 's|nf_|nc_|' 

vi /usr/include/netcdf.h

nc_clobber
nc_close
  int nc_close(int ncid);
nc_create
  int nc_create(const char *path, int cmode, int *ncidp);
nc_def_dim
  int nc_def_dim(int ncid, const char *name, size_t len, int *idp);
nc_def_var
  int nc_def_var(int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp);
nc_double
nc_enddef
  int nc_enddef(int ncid);
nc_float
nc_get_att_int
  int nc_get_att_int(int ncid, int varid, const char *name, int *ip);
nc_get_vara_double nc_get_vara_int nc_get_vara_real
  int nc_get_vara_double(int ncid, int varid, const size_t *startp, const size_t *countp, double *ip);
nc_global
nc_inq
  int nc_inq(int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp);
nc_inq_att
  int nc_inq_att(int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp);
nc_inq_dim
  int nc_inq_dim(int ncid, int dimid, char *name, size_t *lenp);
nc_inq_dimlen
  int nc_inq_dimlen(int ncid, int dimid, size_t *lenp);
nc_inq_var
  int nc_inq_var(int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp);
nc_int
nc_max_name
nc_max_var_dims
nc_noerr
nc_open
  int nc_open(const char *path, int mode, int *ncidp);
nc_put_att_double, nc_put_att_int, nc_put_att_real
  int nc_put_att_double(int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op);
nc_put_att_text
  int nc_put_att_text(int ncid, int varid, const char *name, size_t len, const char *op);
nc_put_vara_double, nc_put_vara_int, nc_put_vara_real
  int nc_put_vara_double(int ncid, int varid, const size_t *startp, const size_t *countp, const double *op);
nc_redef
  int nc_redef(int ncid);
nc_sync
  int nc_sync(int ncid);
nc_unlimited
nc_write

nc_strerror
  const char * nc_strerror(int ncerr);

*/

#include "netcdf.h"

/***********************************************
 *  PREF   = { "", "_", "__" }
 *  SUFF   = { "", "_", "__" }
 *  FTYPE  = { "DOUBLE", "REAL",  "INT" }
 *  CTYPE  = { "double", "float", "int" }
 *
 */


/*=====  PREF, SUFF, FTYPE, CTYPE  =====
 *
 *  #define MNCS_ ( PREF, SUFF, FTYPE, CTYPE ) \
 *    int PREF ## MNCS_ ## FTYPE ## SUFF (  ) \
 *    { return PREF ## nc_ ## CTYPE ## SUFF (  ); }
 *
 */

#define MNCS_GET_ATT_( PREF, SUFF, FTYPE, CTYPE ) \
  int PREF ## MNCS_GET_ATT_ ## FTYPE ## SUFF ( int ncid, int varid, const char *name, CTYPE *ip ) \
  { return PREF ## nc_get_att_ ## CTYPE ## SUFF ( ncid, varid, name, ip ); }

#define MNCS_GET_VARA_( PREF, SUFF, FTYPE, CTYPE ) \
  int PREF ## MNCS_GET_VARA_ ## FTYPE ## SUFF ( int ncid, int varid, const size_t *startp, const size_t *countp, CTYPE *ip ) \
  { return PREF ## nc_get_vara_ ## CTYPE ## SUFF ( ncid, varid, startp, countp, ip ); }

#define MNCS_PUT_ATT_( PREF, SUFF, FTYPE, CTYPE ) \
  int PREF ## MNCS_PUT_ATT_ ## FTYPE ## SUFF ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const CTYPE *op ) \
  { return PREF ## nc_put_att_ ## CTYPE ## SUFF ( ncid, varid, name, xtype, len, op ); }

#define MNCS_PUT_VARA_( PREF, SUFF, FTYPE, CTYPE ) \
  int PREF ## MNCS_PUT_VARA_ ## FTYPE ## SUFF ( int ncid, int varid, const size_t *startp, const size_t *countp, const CTYPE *op ) \
  { return PREF ## nc_put_vara_ ## CTYPE ## SUFF ( ncid, varid, startp, countp, op ); }


/*=====  PREF, SUFF  =====
 *
 *  #define MNCS_ ( PREF, SUFF ) \
 *    int PREF ## MNCS_ ## SUFF (  ) \
 *    { return PREF ## nc_ ## SUFF (  ); }
 *
 */

#define MNCS_CLOSE( PREF, SUFF ) \
  int PREF ## MNCS_CLOSE ## SUFF ( int ncid ) \
  { return PREF ## nc_close ## SUFF ( ncid ); }

#define MNCS_CREATE( PREF, SUFF ) \
  int PREF ## MNCS_CREATE ## SUFF ( const char *path, int cmode, int *ncidp ) \
  { return PREF ## nc_create ## SUFF ( path, cmode, ncidp ); }

#define MNCS_DEF_DIM( PREF, SUFF ) \
  int PREF ## MNCS_DEF_DIM ## SUFF ( int ncid, const char *name, size_t len, int *idp ) \
  { return PREF ## nc_def_dim ## SUFF ( ncid, name, len, idp ); }

#define MNCS_DEF_VAR( PREF, SUFF ) \
  int PREF ## MNCS_DEF_VAR ## SUFF ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) \
  { return PREF ## nc_def_var ## SUFF ( ncid, name, xtype, ndims, dimidsp, varidp ); }

#define MNCS_ENDDEF( PREF, SUFF ) \
  int PREF ## MNCS_ENDDEF ## SUFF ( int ncid ) \
  { return PREF ## nc_enddef ## SUFF ( ncid ); }

#define MNCS_INQ( PREF, SUFF ) \
  int PREF ## MNCS_INQ ## SUFF ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) \
  { return PREF ## nc_inq ## SUFF ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }

#define MNCS_INQ_ATT( PREF, SUFF ) \
  int PREF ## MNCS_INQ_ATT ## SUFF ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) \
  { return PREF ## nc_inq_att ## SUFF ( ncid, varid, name, xtypep, lenp ); }

#define MNCS_INQ_DIMLEN( PREF, SUFF ) \
  int PREF ## MNCS_INQ_DIMLEN ## SUFF ( int ncid, int dimid, size_t *lenp ) \
  { return PREF ## nc_inq_dimlen ## SUFF ( ncid, dimid, lenp ); }

#define MNCS_INQ_VAR( PREF, SUFF ) \
  int PREF ## MNCS_INQ_VAR ## SUFF ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) \
  { return PREF ## nc_inq_var ## SUFF ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }

#define MNCS_OPEN( PREF, SUFF ) \
  int PREF ## MNCS_OPEN ## SUFF ( const char *path, int mode, int *ncidp ) \
  { return PREF ## nc_open ## SUFF ( path, mode, ncidp ); }

#define MNCS_SYNC( PREF, SUFF ) \
  int PREF ## MNCS_SYNC ## SUFF ( int ncid ) \
  { return PREF ## nc_sync ## SUFF ( ncid ); }

#define MNCS_REDEF( PREF, SUFF ) \
  int PREF ## MNCS_REDEF ## SUFF ( int ncid ) \
  { return PREF ## nc_redef ## SUFF ( ncid ); }


/* NOTE: non-int return type */

#define MNCS_STRERROR( PREF, SUFF ) \
  const char * PREF ## MNCS_STRERROR ## SUFF ( int ncerr ) \
  { return (const char *) PREF ## nc_strerror ## SUFF ( ncerr ); }


/*=== end of header ===*/

