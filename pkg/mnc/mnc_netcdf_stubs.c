typedef int ptrdiff_t;
typedef unsigned int size_t;
typedef long int wchar_t;

extern int *__errno_location (void) ;

typedef enum {
        NC_NAT = 0,
        NC_BYTE = 1,
        NC_CHAR = 2,
        NC_SHORT = 3,
        NC_INT = 4,
        NC_FLOAT = 5,
        NC_DOUBLE = 6
} nc_type;
extern const char *
nc_inq_libvers(void);

extern const char *
nc_strerror(int ncerr);

extern int
nc__create(const char *path, int cmode, size_t initialsz,
         size_t *chunksizehintp, int *ncidp);

extern int
nc_create(const char *path, int cmode, int *ncidp);

extern int
nc__open(const char *path, int mode,
        size_t *chunksizehintp, int *ncidp);

extern int
nc_open(const char *path, int mode, int *ncidp);

extern int
nc_set_fill(int ncid, int fillmode, int *old_modep);

extern int
nc_redef(int ncid);

extern int
nc__enddef(int ncid, size_t h_minfree, size_t v_align,
        size_t v_minfree, size_t r_align);

extern int
nc_enddef(int ncid);

extern int
nc_sync(int ncid);

extern int
nc_abort(int ncid);

extern int
nc_close(int ncid);

extern int
nc_inq(int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp);

extern int
nc_inq_ndims(int ncid, int *ndimsp);

extern int
nc_inq_nvars(int ncid, int *nvarsp);

extern int
nc_inq_natts(int ncid, int *nattsp);

extern int
nc_inq_unlimdim(int ncid, int *unlimdimidp);



extern int
nc_def_dim(int ncid, const char *name, size_t len, int *idp);

extern int
nc_inq_dimid(int ncid, const char *name, int *idp);

extern int
nc_inq_dim(int ncid, int dimid, char *name, size_t *lenp);

extern int
nc_inq_dimname(int ncid, int dimid, char *name);

extern int
nc_inq_dimlen(int ncid, int dimid, size_t *lenp);

extern int
nc_rename_dim(int ncid, int dimid, const char *name);




extern int
nc_inq_att(int ncid, int varid, const char *name,
         nc_type *xtypep, size_t *lenp);

extern int
nc_inq_attid(int ncid, int varid, const char *name, int *idp);

extern int
nc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep);

extern int
nc_inq_attlen(int ncid, int varid, const char *name, size_t *lenp);

extern int
nc_inq_attname(int ncid, int varid, int attnum, char *name);

extern int
nc_copy_att(int ncid_in, int varid_in, const char *name, int ncid_out, int varid_out);

extern int
nc_rename_att(int ncid, int varid, const char *name, const char *newname);

extern int
nc_del_att(int ncid, int varid, const char *name);




extern int
nc_put_att_text(int ncid, int varid, const char *name,
        size_t len, const char *op);

extern int
nc_get_att_text(int ncid, int varid, const char *name, char *ip);

extern int
nc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const unsigned char *op);

extern int
nc_get_att_uchar(int ncid, int varid, const char *name, unsigned char *ip);

extern int
nc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const signed char *op);

extern int
nc_get_att_schar(int ncid, int varid, const char *name, signed char *ip);

extern int
nc_put_att_short(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const short *op);

extern int
nc_get_att_short(int ncid, int varid, const char *name, short *ip);

extern int
nc_put_att_int(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const int *op);

extern int
nc_get_att_int(int ncid, int varid, const char *name, int *ip);

extern int
nc_put_att_long(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const long *op);

extern int
nc_get_att_long(int ncid, int varid, const char *name, long *ip);

extern int
nc_put_att_float(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const float *op);

extern int
nc_get_att_float(int ncid, int varid, const char *name, float *ip);

extern int
nc_put_att_double(int ncid, int varid, const char *name, nc_type xtype,
        size_t len, const double *op);

extern int
nc_get_att_double(int ncid, int varid, const char *name, double *ip);




extern int
nc_def_var(int ncid, const char *name,
         nc_type xtype, int ndims, const int *dimidsp, int *varidp);

extern int
nc_inq_var(int ncid, int varid, char *name,
         nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp);

extern int
nc_inq_varid(int ncid, const char *name, int *varidp);

extern int
nc_inq_varname(int ncid, int varid, char *name);

extern int
nc_inq_vartype(int ncid, int varid, nc_type *xtypep);

extern int
nc_inq_varndims(int ncid, int varid, int *ndimsp);

extern int
nc_inq_vardimid(int ncid, int varid, int *dimidsp);

extern int
nc_inq_varnatts(int ncid, int varid, int *nattsp);

extern int
nc_rename_var(int ncid, int varid, const char *name);

extern int
nc_copy_var(int ncid_in, int varid, int ncid_out);
extern int
nc_put_var1_text(int ncid, int varid, const size_t *indexp, const char *op);

extern int
nc_get_var1_text(int ncid, int varid, const size_t *indexp, char *ip);

extern int
nc_put_var1_uchar(int ncid, int varid, const size_t *indexp,
        const unsigned char *op);

extern int
nc_get_var1_uchar(int ncid, int varid, const size_t *indexp,
        unsigned char *ip);

extern int
nc_put_var1_schar(int ncid, int varid, const size_t *indexp,
        const signed char *op);

extern int
nc_get_var1_schar(int ncid, int varid, const size_t *indexp,
        signed char *ip);

extern int
nc_put_var1_short(int ncid, int varid, const size_t *indexp,
        const short *op);

extern int
nc_get_var1_short(int ncid, int varid, const size_t *indexp,
        short *ip);

extern int
nc_put_var1_int(int ncid, int varid, const size_t *indexp, const int *op);

extern int
nc_get_var1_int(int ncid, int varid, const size_t *indexp, int *ip);

extern int
nc_put_var1_long(int ncid, int varid, const size_t *indexp, const long *op);

extern int
nc_get_var1_long(int ncid, int varid, const size_t *indexp, long *ip);

extern int
nc_put_var1_float(int ncid, int varid, const size_t *indexp, const float *op);

extern int
nc_get_var1_float(int ncid, int varid, const size_t *indexp, float *ip);

extern int
nc_put_var1_double(int ncid, int varid, const size_t *indexp, const double *op);

extern int
nc_get_var1_double(int ncid, int varid, const size_t *indexp, double *ip);




extern int
nc_put_vara_text(int ncid, int varid,
        const size_t *startp, const size_t *countp, const char *op);

extern int
nc_get_vara_text(int ncid, int varid,
        const size_t *startp, const size_t *countp, char *ip);

extern int
nc_put_vara_uchar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const unsigned char *op);

extern int
nc_get_vara_uchar(int ncid, int varid,
        const size_t *startp, const size_t *countp, unsigned char *ip);

extern int
nc_put_vara_schar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const signed char *op);

extern int
nc_get_vara_schar(int ncid, int varid,
        const size_t *startp, const size_t *countp, signed char *ip);

extern int
nc_put_vara_short(int ncid, int varid,
        const size_t *startp, const size_t *countp, const short *op);

extern int
nc_get_vara_short(int ncid, int varid,
        const size_t *startp, const size_t *countp, short *ip);

extern int
nc_put_vara_int(int ncid, int varid,
        const size_t *startp, const size_t *countp, const int *op);

extern int
nc_get_vara_int(int ncid, int varid,
        const size_t *startp, const size_t *countp, int *ip);

extern int
nc_put_vara_long(int ncid, int varid,
        const size_t *startp, const size_t *countp, const long *op);

extern int
nc_get_vara_long(int ncid, int varid,
        const size_t *startp, const size_t *countp, long *ip);

extern int
nc_put_vara_float(int ncid, int varid,
        const size_t *startp, const size_t *countp, const float *op);

extern int
nc_get_vara_float(int ncid, int varid,
        const size_t *startp, const size_t *countp, float *ip);

extern int
nc_put_vara_double(int ncid, int varid,
        const size_t *startp, const size_t *countp, const double *op);

extern int
nc_get_vara_double(int ncid, int varid,
        const size_t *startp, const size_t *countp, double *ip);




extern int
nc_put_vars_text(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const char *op);

extern int
nc_get_vars_text(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        char *ip);

extern int
nc_put_vars_uchar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const unsigned char *op);

extern int
nc_get_vars_uchar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        unsigned char *ip);

extern int
nc_put_vars_schar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const signed char *op);

extern int
nc_get_vars_schar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        signed char *ip);

extern int
nc_put_vars_short(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const short *op);

extern int
nc_get_vars_short(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        short *ip);

extern int
nc_put_vars_int(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const int *op);

extern int
nc_get_vars_int(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        int *ip);

extern int
nc_put_vars_long(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const long *op);

extern int
nc_get_vars_long(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        long *ip);

extern int
nc_put_vars_float(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const float *op);

extern int
nc_get_vars_float(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        float *ip);

extern int
nc_put_vars_double(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const double *op);

extern int
nc_get_vars_double(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        double *ip);




extern int
nc_put_varm_text(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const char *op);

extern int
nc_get_varm_text(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        char *ip);

extern int
nc_put_varm_uchar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const unsigned char *op);

extern int
nc_get_varm_uchar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        unsigned char *ip);

extern int
nc_put_varm_schar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const signed char *op);

extern int
nc_get_varm_schar(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        signed char *ip);

extern int
nc_put_varm_short(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const short *op);

extern int
nc_get_varm_short(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        short *ip);

extern int
nc_put_varm_int(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const int *op);

extern int
nc_get_varm_int(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        int *ip);

extern int
nc_put_varm_long(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const long *op);

extern int
nc_get_varm_long(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        long *ip);

extern int
nc_put_varm_float(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const float *op);

extern int
nc_get_varm_float(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        float *ip);

extern int
nc_put_varm_double(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t *imapp,
        const double *op);

extern int
nc_get_varm_double(int ncid, int varid,
        const size_t *startp, const size_t *countp, const ptrdiff_t *stridep,
        const ptrdiff_t * imap,
        double *ip);




extern int
nc_put_var_text(int ncid, int varid, const char *op);

extern int
nc_get_var_text(int ncid, int varid, char *ip);

extern int
nc_put_var_uchar(int ncid, int varid, const unsigned char *op);

extern int
nc_get_var_uchar(int ncid, int varid, unsigned char *ip);

extern int
nc_put_var_schar(int ncid, int varid, const signed char *op);

extern int
nc_get_var_schar(int ncid, int varid, signed char *ip);

extern int
nc_put_var_short(int ncid, int varid, const short *op);

extern int
nc_get_var_short(int ncid, int varid, short *ip);

extern int
nc_put_var_int(int ncid, int varid, const int *op);

extern int
nc_get_var_int(int ncid, int varid, int *ip);

extern int
nc_put_var_long(int ncid, int varid, const long *op);

extern int
nc_get_var_long(int ncid, int varid, long *ip);

extern int
nc_put_var_float(int ncid, int varid, const float *op);

extern int
nc_get_var_float(int ncid, int varid, float *ip);

extern int
nc_put_var_double(int ncid, int varid, const double *op);

extern int
nc_get_var_double(int ncid, int varid, double *ip);
extern int
nc__create_mp(const char *path, int cmode, size_t initialsz, int basepe,
         size_t *chunksizehintp, int *ncidp);

extern int
nc__open_mp(const char *path, int mode, int basepe,
        size_t *chunksizehintp, int *ncidp);

extern int
nc_delete_mp(const char * path, int basepe);

extern int
nc_set_base_pe(int ncid, int pe);

extern int
nc_inq_base_pe(int ncid, int *pe);
extern int ncerr;
extern int ncopts;

extern void
nc_advise(const char *cdf_routine_name, int err, const char *fmt,...);







typedef int nclong;

extern int
nctypelen(nc_type datatype);

extern int
nccreate(const char* path, int cmode);

extern int
ncopen(const char* path, int mode);

extern int
ncsetfill(int ncid, int fillmode);

extern int
ncredef(int ncid);

extern int
ncendef(int ncid);

extern int
ncsync(int ncid);

extern int
ncabort(int ncid);

extern int
ncclose(int ncid);

extern int
ncinquire(int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimp);

extern int
ncdimdef(int ncid, const char *name, long len);

extern int
ncdimid(int ncid, const char *name);

extern int
ncdiminq(int ncid, int dimid, char *name, long *lenp);

extern int
ncdimrename(int ncid, int dimid, const char *name);

extern int
ncattput(int ncid, int varid, const char *name, nc_type xtype,
        int len, const void *op);

extern int
ncattinq(int ncid, int varid, const char *name, nc_type *xtypep, int *lenp);

extern int
ncattget(int ncid, int varid, const char *name, void *ip);

extern int
ncattcopy(int ncid_in, int varid_in, const char *name, int ncid_out,
        int varid_out);

extern int
ncattname(int ncid, int varid, int attnum, char *name);

extern int
ncattrename(int ncid, int varid, const char *name, const char *newname);

extern int
ncattdel(int ncid, int varid, const char *name);

extern int
ncvardef(int ncid, const char *name, nc_type xtype,
        int ndims, const int *dimidsp);

extern int
ncvarid(int ncid, const char *name);

extern int
ncvarinq(int ncid, int varid, char *name, nc_type *xtypep,
        int *ndimsp, int *dimidsp, int *nattsp);

extern int
ncvarput1(int ncid, int varid, const long *indexp, const void *op);

extern int
ncvarget1(int ncid, int varid, const long *indexp, void *ip);

extern int
ncvarput(int ncid, int varid, const long *startp, const long *countp,
        const void *op);

extern int
ncvarget(int ncid, int varid, const long *startp, const long *countp,
        void *ip);

extern int
ncvarputs(int ncid, int varid, const long *startp, const long *countp,
        const long *stridep, const void *op);

extern int
ncvargets(int ncid, int varid, const long *startp, const long *countp,
        const long *stridep, void *ip);

extern int
ncvarputg(int ncid, int varid, const long *startp, const long *countp,
        const long *stridep, const long *imapp, const void *op);

extern int
ncvargetg(int ncid, int varid, const long *startp, const long *countp,
        const long *stridep, const long *imapp, void *ip);

extern int
ncvarrename(int ncid, int varid, const char *name);

extern int
ncrecinq(int ncid, int *nrecvarsp, int *recvaridsp, long *recsizesp);

extern int
ncrecget(int ncid, long recnum, void **datap);

extern int
ncrecput(int ncid, long recnum, void *const *datap);
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
