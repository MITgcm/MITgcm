
#include "netcdf.h"

int MNCS_CLOSE ( int ncid ) { return nc_close ( ncid ); }
int MNCS_CLOSE_ ( int ncid ) { return nc_close_ ( ncid ); }
int MNCS_CLOSE__ ( int ncid ) { return nc_close__ ( ncid ); }
int _MNCS_CLOSE ( int ncid ) { return _nc_close ( ncid ); }
int _MNCS_CLOSE_ ( int ncid ) { return _nc_close_ ( ncid ); }
int _MNCS_CLOSE__ ( int ncid ) { return _nc_close__ ( ncid ); }
int __MNCS_CLOSE ( int ncid ) { return __nc_close ( ncid ); }
int __MNCS_CLOSE_ ( int ncid ) { return __nc_close_ ( ncid ); }
int __MNCS_CLOSE__ ( int ncid ) { return __nc_close__ ( ncid ); }
int MNCS_CREATE ( const char *path, int cmode, int *ncidp ) { return nc_create ( path, cmode, ncidp ); }
int MNCS_CREATE_ ( const char *path, int cmode, int *ncidp ) { return nc_create_ ( path, cmode, ncidp ); }
int MNCS_CREATE__ ( const char *path, int cmode, int *ncidp ) { return nc_create__ ( path, cmode, ncidp ); }
int _MNCS_CREATE ( const char *path, int cmode, int *ncidp ) { return _nc_create ( path, cmode, ncidp ); }
int _MNCS_CREATE_ ( const char *path, int cmode, int *ncidp ) { return _nc_create_ ( path, cmode, ncidp ); }
int _MNCS_CREATE__ ( const char *path, int cmode, int *ncidp ) { return _nc_create__ ( path, cmode, ncidp ); }
int __MNCS_CREATE ( const char *path, int cmode, int *ncidp ) { return __nc_create ( path, cmode, ncidp ); }
int __MNCS_CREATE_ ( const char *path, int cmode, int *ncidp ) { return __nc_create_ ( path, cmode, ncidp ); }
int __MNCS_CREATE__ ( const char *path, int cmode, int *ncidp ) { return __nc_create__ ( path, cmode, ncidp ); }
int MNCS_DEF_DIM ( int ncid, const char *name, size_t len, int *idp ) { return nc_def_dim ( ncid, name, len, idp ); }
int MNCS_DEF_DIM_ ( int ncid, const char *name, size_t len, int *idp ) { return nc_def_dim_ ( ncid, name, len, idp ); }
int MNCS_DEF_DIM__ ( int ncid, const char *name, size_t len, int *idp ) { return nc_def_dim__ ( ncid, name, len, idp ); }
int _MNCS_DEF_DIM ( int ncid, const char *name, size_t len, int *idp ) { return _nc_def_dim ( ncid, name, len, idp ); }
int _MNCS_DEF_DIM_ ( int ncid, const char *name, size_t len, int *idp ) { return _nc_def_dim_ ( ncid, name, len, idp ); }
int _MNCS_DEF_DIM__ ( int ncid, const char *name, size_t len, int *idp ) { return _nc_def_dim__ ( ncid, name, len, idp ); }
int __MNCS_DEF_DIM ( int ncid, const char *name, size_t len, int *idp ) { return __nc_def_dim ( ncid, name, len, idp ); }
int __MNCS_DEF_DIM_ ( int ncid, const char *name, size_t len, int *idp ) { return __nc_def_dim_ ( ncid, name, len, idp ); }
int __MNCS_DEF_DIM__ ( int ncid, const char *name, size_t len, int *idp ) { return __nc_def_dim__ ( ncid, name, len, idp ); }
int MNCS_DEF_VAR ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return nc_def_var ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int MNCS_DEF_VAR_ ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return nc_def_var_ ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int MNCS_DEF_VAR__ ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return nc_def_var__ ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int _MNCS_DEF_VAR ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return _nc_def_var ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int _MNCS_DEF_VAR_ ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return _nc_def_var_ ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int _MNCS_DEF_VAR__ ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return _nc_def_var__ ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int __MNCS_DEF_VAR ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return __nc_def_var ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int __MNCS_DEF_VAR_ ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return __nc_def_var_ ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int __MNCS_DEF_VAR__ ( int ncid, const char *name, nc_type xtype, int ndims, const int *dimidsp, int *varidp ) { return __nc_def_var__ ( ncid, name, xtype, ndims, dimidsp, varidp ); }
int MNCS_ENDDEF ( int ncid ) { return nc_enddef ( ncid ); }
int MNCS_ENDDEF_ ( int ncid ) { return nc_enddef_ ( ncid ); }
int MNCS_ENDDEF__ ( int ncid ) { return nc_enddef__ ( ncid ); }
int _MNCS_ENDDEF ( int ncid ) { return _nc_enddef ( ncid ); }
int _MNCS_ENDDEF_ ( int ncid ) { return _nc_enddef_ ( ncid ); }
int _MNCS_ENDDEF__ ( int ncid ) { return _nc_enddef__ ( ncid ); }
int __MNCS_ENDDEF ( int ncid ) { return __nc_enddef ( ncid ); }
int __MNCS_ENDDEF_ ( int ncid ) { return __nc_enddef_ ( ncid ); }
int __MNCS_ENDDEF__ ( int ncid ) { return __nc_enddef__ ( ncid ); }
int MNCS_INQ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return nc_inq ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int MNCS_INQ_ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return nc_inq_ ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int MNCS_INQ__ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return nc_inq__ ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int _MNCS_INQ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return _nc_inq ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int _MNCS_INQ_ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return _nc_inq_ ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int _MNCS_INQ__ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return _nc_inq__ ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int __MNCS_INQ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return __nc_inq ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int __MNCS_INQ_ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return __nc_inq_ ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int __MNCS_INQ__ ( int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp ) { return __nc_inq__ ( ncid, ndimsp, nvarsp, nattsp, unlimdimidp ); }
int MNCS_INQ_ATT ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return nc_inq_att ( ncid, varid, name, xtypep, lenp ); }
int MNCS_INQ_ATT_ ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return nc_inq_att_ ( ncid, varid, name, xtypep, lenp ); }
int MNCS_INQ_ATT__ ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return nc_inq_att__ ( ncid, varid, name, xtypep, lenp ); }
int _MNCS_INQ_ATT ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return _nc_inq_att ( ncid, varid, name, xtypep, lenp ); }
int _MNCS_INQ_ATT_ ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return _nc_inq_att_ ( ncid, varid, name, xtypep, lenp ); }
int _MNCS_INQ_ATT__ ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return _nc_inq_att__ ( ncid, varid, name, xtypep, lenp ); }
int __MNCS_INQ_ATT ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return __nc_inq_att ( ncid, varid, name, xtypep, lenp ); }
int __MNCS_INQ_ATT_ ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return __nc_inq_att_ ( ncid, varid, name, xtypep, lenp ); }
int __MNCS_INQ_ATT__ ( int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp ) { return __nc_inq_att__ ( ncid, varid, name, xtypep, lenp ); }
int MNCS_INQ_DIMLEN ( int ncid, int dimid, size_t *lenp ) { return nc_inq_dimlen ( ncid, dimid, lenp ); }
int MNCS_INQ_DIMLEN_ ( int ncid, int dimid, size_t *lenp ) { return nc_inq_dimlen_ ( ncid, dimid, lenp ); }
int MNCS_INQ_DIMLEN__ ( int ncid, int dimid, size_t *lenp ) { return nc_inq_dimlen__ ( ncid, dimid, lenp ); }
int _MNCS_INQ_DIMLEN ( int ncid, int dimid, size_t *lenp ) { return _nc_inq_dimlen ( ncid, dimid, lenp ); }
int _MNCS_INQ_DIMLEN_ ( int ncid, int dimid, size_t *lenp ) { return _nc_inq_dimlen_ ( ncid, dimid, lenp ); }
int _MNCS_INQ_DIMLEN__ ( int ncid, int dimid, size_t *lenp ) { return _nc_inq_dimlen__ ( ncid, dimid, lenp ); }
int __MNCS_INQ_DIMLEN ( int ncid, int dimid, size_t *lenp ) { return __nc_inq_dimlen ( ncid, dimid, lenp ); }
int __MNCS_INQ_DIMLEN_ ( int ncid, int dimid, size_t *lenp ) { return __nc_inq_dimlen_ ( ncid, dimid, lenp ); }
int __MNCS_INQ_DIMLEN__ ( int ncid, int dimid, size_t *lenp ) { return __nc_inq_dimlen__ ( ncid, dimid, lenp ); }
int MNCS_INQ_VAR ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return nc_inq_var ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int MNCS_INQ_VAR_ ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return nc_inq_var_ ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int MNCS_INQ_VAR__ ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return nc_inq_var__ ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int _MNCS_INQ_VAR ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return _nc_inq_var ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int _MNCS_INQ_VAR_ ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return _nc_inq_var_ ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int _MNCS_INQ_VAR__ ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return _nc_inq_var__ ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int __MNCS_INQ_VAR ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return __nc_inq_var ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int __MNCS_INQ_VAR_ ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return __nc_inq_var_ ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int __MNCS_INQ_VAR__ ( int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp ) { return __nc_inq_var__ ( ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp ); }
int MNCS_OPEN ( const char *path, int mode, int *ncidp ) { return nc_open ( path, mode, ncidp ); }
int MNCS_OPEN_ ( const char *path, int mode, int *ncidp ) { return nc_open_ ( path, mode, ncidp ); }
int MNCS_OPEN__ ( const char *path, int mode, int *ncidp ) { return nc_open__ ( path, mode, ncidp ); }
int _MNCS_OPEN ( const char *path, int mode, int *ncidp ) { return _nc_open ( path, mode, ncidp ); }
int _MNCS_OPEN_ ( const char *path, int mode, int *ncidp ) { return _nc_open_ ( path, mode, ncidp ); }
int _MNCS_OPEN__ ( const char *path, int mode, int *ncidp ) { return _nc_open__ ( path, mode, ncidp ); }
int __MNCS_OPEN ( const char *path, int mode, int *ncidp ) { return __nc_open ( path, mode, ncidp ); }
int __MNCS_OPEN_ ( const char *path, int mode, int *ncidp ) { return __nc_open_ ( path, mode, ncidp ); }
int __MNCS_OPEN__ ( const char *path, int mode, int *ncidp ) { return __nc_open__ ( path, mode, ncidp ); }
int MNCS_SYNC ( int ncid ) { return nc_sync ( ncid ); }
int MNCS_SYNC_ ( int ncid ) { return nc_sync_ ( ncid ); }
int MNCS_SYNC__ ( int ncid ) { return nc_sync__ ( ncid ); }
int _MNCS_SYNC ( int ncid ) { return _nc_sync ( ncid ); }
int _MNCS_SYNC_ ( int ncid ) { return _nc_sync_ ( ncid ); }
int _MNCS_SYNC__ ( int ncid ) { return _nc_sync__ ( ncid ); }
int __MNCS_SYNC ( int ncid ) { return __nc_sync ( ncid ); }
int __MNCS_SYNC_ ( int ncid ) { return __nc_sync_ ( ncid ); }
int __MNCS_SYNC__ ( int ncid ) { return __nc_sync__ ( ncid ); }
int MNCS_REDEF ( int ncid ) { return nc_redef ( ncid ); }
int MNCS_REDEF_ ( int ncid ) { return nc_redef_ ( ncid ); }
int MNCS_REDEF__ ( int ncid ) { return nc_redef__ ( ncid ); }
int _MNCS_REDEF ( int ncid ) { return _nc_redef ( ncid ); }
int _MNCS_REDEF_ ( int ncid ) { return _nc_redef_ ( ncid ); }
int _MNCS_REDEF__ ( int ncid ) { return _nc_redef__ ( ncid ); }
int __MNCS_REDEF ( int ncid ) { return __nc_redef ( ncid ); }
int __MNCS_REDEF_ ( int ncid ) { return __nc_redef_ ( ncid ); }
int __MNCS_REDEF__ ( int ncid ) { return __nc_redef__ ( ncid ); }
const char * MNCS_STRERROR ( int ncerr ) { return (const char *) nc_strerror ( ncerr ); }
const char * MNCS_STRERROR_ ( int ncerr ) { return (const char *) nc_strerror_ ( ncerr ); }
const char * MNCS_STRERROR__ ( int ncerr ) { return (const char *) nc_strerror__ ( ncerr ); }
const char * _MNCS_STRERROR ( int ncerr ) { return (const char *) _nc_strerror ( ncerr ); }
const char * _MNCS_STRERROR_ ( int ncerr ) { return (const char *) _nc_strerror_ ( ncerr ); }
const char * _MNCS_STRERROR__ ( int ncerr ) { return (const char *) _nc_strerror__ ( ncerr ); }
const char * __MNCS_STRERROR ( int ncerr ) { return (const char *) __nc_strerror ( ncerr ); }
const char * __MNCS_STRERROR_ ( int ncerr ) { return (const char *) __nc_strerror_ ( ncerr ); }
const char * __MNCS_STRERROR__ ( int ncerr ) { return (const char *) __nc_strerror__ ( ncerr ); }
int MNCS_GET_ATT_DOUBLE ( int ncid, int varid, const char *name, double *ip ) { return nc_get_att_double ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_DOUBLE_ ( int ncid, int varid, const char *name, double *ip ) { return nc_get_att_double_ ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_DOUBLE__ ( int ncid, int varid, const char *name, double *ip ) { return nc_get_att_double__ ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_DOUBLE ( int ncid, int varid, const char *name, double *ip ) { return _nc_get_att_double ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_DOUBLE_ ( int ncid, int varid, const char *name, double *ip ) { return _nc_get_att_double_ ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_DOUBLE__ ( int ncid, int varid, const char *name, double *ip ) { return _nc_get_att_double__ ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_DOUBLE ( int ncid, int varid, const char *name, double *ip ) { return __nc_get_att_double ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_DOUBLE_ ( int ncid, int varid, const char *name, double *ip ) { return __nc_get_att_double_ ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_DOUBLE__ ( int ncid, int varid, const char *name, double *ip ) { return __nc_get_att_double__ ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_REAL ( int ncid, int varid, const char *name, float *ip ) { return nc_get_att_float ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_REAL_ ( int ncid, int varid, const char *name, float *ip ) { return nc_get_att_float_ ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_REAL__ ( int ncid, int varid, const char *name, float *ip ) { return nc_get_att_float__ ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_REAL ( int ncid, int varid, const char *name, float *ip ) { return _nc_get_att_float ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_REAL_ ( int ncid, int varid, const char *name, float *ip ) { return _nc_get_att_float_ ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_REAL__ ( int ncid, int varid, const char *name, float *ip ) { return _nc_get_att_float__ ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_REAL ( int ncid, int varid, const char *name, float *ip ) { return __nc_get_att_float ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_REAL_ ( int ncid, int varid, const char *name, float *ip ) { return __nc_get_att_float_ ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_REAL__ ( int ncid, int varid, const char *name, float *ip ) { return __nc_get_att_float__ ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_INT ( int ncid, int varid, const char *name, int *ip ) { return nc_get_att_int ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_INT_ ( int ncid, int varid, const char *name, int *ip ) { return nc_get_att_int_ ( ncid, varid, name, ip ); }
int MNCS_GET_ATT_INT__ ( int ncid, int varid, const char *name, int *ip ) { return nc_get_att_int__ ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_INT ( int ncid, int varid, const char *name, int *ip ) { return _nc_get_att_int ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_INT_ ( int ncid, int varid, const char *name, int *ip ) { return _nc_get_att_int_ ( ncid, varid, name, ip ); }
int _MNCS_GET_ATT_INT__ ( int ncid, int varid, const char *name, int *ip ) { return _nc_get_att_int__ ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_INT ( int ncid, int varid, const char *name, int *ip ) { return __nc_get_att_int ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_INT_ ( int ncid, int varid, const char *name, int *ip ) { return __nc_get_att_int_ ( ncid, varid, name, ip ); }
int __MNCS_GET_ATT_INT__ ( int ncid, int varid, const char *name, int *ip ) { return __nc_get_att_int__ ( ncid, varid, name, ip ); }
int MNCS_GET_VARA_DOUBLE ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return nc_get_vara_double ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_DOUBLE_ ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return nc_get_vara_double_ ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_DOUBLE__ ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return nc_get_vara_double__ ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_DOUBLE ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return _nc_get_vara_double ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_DOUBLE_ ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return _nc_get_vara_double_ ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_DOUBLE__ ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return _nc_get_vara_double__ ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_DOUBLE ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return __nc_get_vara_double ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_DOUBLE_ ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return __nc_get_vara_double_ ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_DOUBLE__ ( int ncid, int varid, const size_t *startp, const size_t *countp, double *ip ) { return __nc_get_vara_double__ ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_REAL ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return nc_get_vara_float ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_REAL_ ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return nc_get_vara_float_ ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_REAL__ ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return nc_get_vara_float__ ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_REAL ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return _nc_get_vara_float ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_REAL_ ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return _nc_get_vara_float_ ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_REAL__ ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return _nc_get_vara_float__ ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_REAL ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return __nc_get_vara_float ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_REAL_ ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return __nc_get_vara_float_ ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_REAL__ ( int ncid, int varid, const size_t *startp, const size_t *countp, float *ip ) { return __nc_get_vara_float__ ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_INT ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return nc_get_vara_int ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_INT_ ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return nc_get_vara_int_ ( ncid, varid, startp, countp, ip ); }
int MNCS_GET_VARA_INT__ ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return nc_get_vara_int__ ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_INT ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return _nc_get_vara_int ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_INT_ ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return _nc_get_vara_int_ ( ncid, varid, startp, countp, ip ); }
int _MNCS_GET_VARA_INT__ ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return _nc_get_vara_int__ ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_INT ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return __nc_get_vara_int ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_INT_ ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return __nc_get_vara_int_ ( ncid, varid, startp, countp, ip ); }
int __MNCS_GET_VARA_INT__ ( int ncid, int varid, const size_t *startp, const size_t *countp, int *ip ) { return __nc_get_vara_int__ ( ncid, varid, startp, countp, ip ); }
int MNCS_PUT_ATT_DOUBLE ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return nc_put_att_double ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_DOUBLE_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return nc_put_att_double_ ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_DOUBLE__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return nc_put_att_double__ ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_DOUBLE ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return _nc_put_att_double ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_DOUBLE_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return _nc_put_att_double_ ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_DOUBLE__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return _nc_put_att_double__ ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_DOUBLE ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return __nc_put_att_double ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_DOUBLE_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return __nc_put_att_double_ ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_DOUBLE__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const double *op ) { return __nc_put_att_double__ ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_REAL ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return nc_put_att_float ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_REAL_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return nc_put_att_float_ ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_REAL__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return nc_put_att_float__ ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_REAL ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return _nc_put_att_float ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_REAL_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return _nc_put_att_float_ ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_REAL__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return _nc_put_att_float__ ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_REAL ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return __nc_put_att_float ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_REAL_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return __nc_put_att_float_ ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_REAL__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const float *op ) { return __nc_put_att_float__ ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_INT ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return nc_put_att_int ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_INT_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return nc_put_att_int_ ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_ATT_INT__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return nc_put_att_int__ ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_INT ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return _nc_put_att_int ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_INT_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return _nc_put_att_int_ ( ncid, varid, name, xtype, len, op ); }
int _MNCS_PUT_ATT_INT__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return _nc_put_att_int__ ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_INT ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return __nc_put_att_int ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_INT_ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return __nc_put_att_int_ ( ncid, varid, name, xtype, len, op ); }
int __MNCS_PUT_ATT_INT__ ( int ncid, int varid, const char *name, nc_type xtype, size_t len, const int *op ) { return __nc_put_att_int__ ( ncid, varid, name, xtype, len, op ); }
int MNCS_PUT_VARA_DOUBLE ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return nc_put_vara_double ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_DOUBLE_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return nc_put_vara_double_ ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_DOUBLE__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return nc_put_vara_double__ ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_DOUBLE ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return _nc_put_vara_double ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_DOUBLE_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return _nc_put_vara_double_ ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_DOUBLE__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return _nc_put_vara_double__ ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_DOUBLE ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return __nc_put_vara_double ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_DOUBLE_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return __nc_put_vara_double_ ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_DOUBLE__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const double *op ) { return __nc_put_vara_double__ ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_REAL ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return nc_put_vara_float ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_REAL_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return nc_put_vara_float_ ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_REAL__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return nc_put_vara_float__ ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_REAL ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return _nc_put_vara_float ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_REAL_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return _nc_put_vara_float_ ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_REAL__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return _nc_put_vara_float__ ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_REAL ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return __nc_put_vara_float ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_REAL_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return __nc_put_vara_float_ ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_REAL__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const float *op ) { return __nc_put_vara_float__ ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_INT ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return nc_put_vara_int ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_INT_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return nc_put_vara_int_ ( ncid, varid, startp, countp, op ); }
int MNCS_PUT_VARA_INT__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return nc_put_vara_int__ ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_INT ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return _nc_put_vara_int ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_INT_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return _nc_put_vara_int_ ( ncid, varid, startp, countp, op ); }
int _MNCS_PUT_VARA_INT__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return _nc_put_vara_int__ ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_INT ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return __nc_put_vara_int ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_INT_ ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return __nc_put_vara_int_ ( ncid, varid, startp, countp, op ); }
int __MNCS_PUT_VARA_INT__ ( int ncid, int varid, const size_t *startp, const size_t *countp, const int *op ) { return __nc_put_vara_int__ ( ncid, varid, startp, countp, op ); }
