#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_NF90IO_VAR_DEF

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_NF90IO_VAR_DEF(
     I     ncid, nLevOutp, listId, ndId,
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Define a variable in a recently created netCDF file.

C     !USES:
#ifdef ALLOW_NF90IO
      use netcdf
#endif

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#ifdef ALLOW_NF90IO
#include "NF90IO.h"
C #include "mpif.h"
C #include "NF90IO.h"
#endif

C     !INPUT PARAMETERS:
C     NrMax           :: 3rd dimension of output-field array to write
C     nLevOutp        :: number of levels to write in output file
C     listId          :: Diagnostics list number being written
C     ndId            :: diagnostics Id number (in available diagnostics list)
C     myTime          :: current time of simulation (s)
C     myIter          :: current iteration number
C     myThid          :: my Thread Id number
      INTEGER ncid
      INTEGER NrMax
      INTEGER nLevOutp
      INTEGER listId, ndId
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_NF90IO

C     !LOCAL VARIABLES:
      INTEGER varid, rec_dimid, i_dimid, j_dimid, k_dimid, err
      INTEGER xtype
      INTEGER dimids(4)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER( myThid )

C logic copied from mnc...
      IF ( ( writeBinaryPrec .EQ. precFloat32 .AND.
     &       fflags(listId)(1:1) .NE. 'D' )
     &     .OR. fflags(listId)(1:1) .EQ. 'R' ) THEN
        xtype = NF90_FLOAT
      ELSE
        xtype = NF90_DOUBLE
      ENDIF

C     we need to initialize this variable
      err = nf90_inq_dimid(ncid, 'record', rec_dimid)
      CALL nf90ERR(err, "inq record_dimid",myThid)

C     gdiag will tell us what the other two or three dimensions are
C     so here we get i_dimid and j_dimid with the proper dimensions
      IF (gdiag(ndId)(2:2) .EQ. 'M') THEN
         err = nf90_inq_dimid(ncid, 'i', i_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
         err = nf90_inq_dimid(ncid, 'j', j_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
      ELSEIF (gdiag(ndId)(2:2) .EQ. 'U') THEN
         err = nf90_inq_dimid(ncid, 'i_g', i_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
         err = nf90_inq_dimid(ncid, 'j', j_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
      ELSEIF (gdiag(ndId)(2:2) .EQ. 'V') THEN
         err = nf90_inq_dimid(ncid, 'i', i_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
         err = nf90_inq_dimid(ncid, 'j_g', j_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
      ELSEIF (gdiag(ndId)(2:2) .EQ. 'Z') THEN
         err = nf90_inq_dimid(ncid, 'i_g', i_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
         err = nf90_inq_dimid(ncid, 'j_g', j_dimid)
         CALL nf90ERR(err, "inq i_dimid",myThid)
      ENDIF

C     do the same for k_dimid (if necessary)
      IF (nLevOutp.EQ.Nr) THEN
         err = nf90_inq_dimid(ncid, 'k', k_dimid)
         CALL nf90ERR(err, "inq k_dimid",myThid)
         IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &        .AND. (gdiag(ndId)(9:9) .EQ. 'M') ) THEN
            err = nf90_inq_dimid(ncid, 'k', k_dimid)
            CALL nf90ERR(err, "inq k_dimid",myThid)
         ENDIF
         IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &        .AND. (gdiag(ndId)(9:9) .EQ. 'L') ) THEN
            err = nf90_inq_dimid(ncid, 'k_l', k_dimid)
            CALL nf90ERR(err, "inq k_dimid",myThid)
         ENDIF
         IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &        .AND. (gdiag(ndId)(9:9) .EQ. 'U') ) THEN
            err = nf90_inq_dimid(ncid, 'k_u', k_dimid)
            CALL nf90ERR(err, "inq k_dimid",myThid)
         ENDIF

         dimids =  (/  i_dimid, j_dimid, k_dimid, rec_dimid /)
         err = nf90_def_var(ncid, cdiag(ndId), xtype, dimids,
     &        varid)
      ELSEIF (nLevOutp.EQ.1) THEN
C        Define the one-level variable
         dimids(1:3) =  (/  i_dimid, j_dimid, rec_dimid /)
         err = nf90_def_var(ncid, cdiag(ndId), xtype, dimids(1:3),
     &        varid)
      ELSE
C        This is a multi-level diagnostic and we need to define a new
C        dimension k_level.
         err = nf90_inq_dimid(ncid, 'k_level', k_dimid)
         IF (.NOT.(err.EQ.nf90_NoErr)) THEN
C          we need to define k_level
            err = nf90_def_dim(ncid, "k_level", nLevOutp,
     &           k_dimid)
            CALL nf90ERR(err,"Adding k_level dim",myThid)
            err = nf90_def_var(ncid, "k_level", NF90_INT, (/ k_dimid
     &           /),varid)
            CALL nf90ERR(err,"Adding k_level variable",myThid)
            err = nf90_put_att(ncid, varid,
     &           "standard_name",
     &           "z_grid_index")
            err = nf90_put_att(ncid, varid,
     &           "long_name",
     &           "z-dimension of the level grid")
CC           Drop out of def mode to fill the values in here
C            err = nf90_enddef(ncid)
C            err = nf90io_var_par_access(ncid, varid)
C            CALL nf90ERR(err, "Setting k variable to par access"
C     &           ,myThid)
C            err = nf90_put_var(ncid, varid, levs(1:nLevOutp, listId),
C     &           start = (/ 1 /) , count = (/ nLevOutp /) )
C            CALL nf90ERR(err, "putting data in  k_level variable"
C     &           ,myThid)
C            err = nf90_redef(ncid)
C            CALL nf90ERR(err, "Put netcdf file back in redef"
C     &           ,myThid)
         ENDIF
C        Done if we need to define k_level
         dimids =  (/  i_dimid, j_dimid, k_dimid, rec_dimid /)
         err = nf90_def_var(ncid, cdiag(ndId), xtype, dimids,
     &        varid)
      ENDIF
      CALL nf90ERR(err, "Adding diagnostic variable",myThid)

C     add attributes to the newy defined variable
      err = nf90_put_att(ncid, varid,
     &     "description",
     &     tdiag(ndId))
      CALL nf90ERR(err, "Adding attribute diagnostic variable"
     &     ,myThid)
      err = nf90_put_att(ncid, varid,
     &     "units",
     &     udiag(ndId))
      CALL nf90ERR(err, "Adding attribute diagnostic variable"
     &     ,myThid)
C     no standard name?  Booo

C     set to not pre-fill variable....
      err = nf90_def_var_fill(ncid, varid, 1, 0)
      CALL nf90ERR(err, "Setting variable not to be pre-filled"
     &     ,myThid)

      _END_MASTER(myThid)

#endif /*  ALLOW_NF90IO  */

      RETURN
      END
