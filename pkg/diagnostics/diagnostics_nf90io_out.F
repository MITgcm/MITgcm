#include "DIAG_OPTIONS.h"

C--   File diagnostics_nf90io_out.F: Routines to write NF90IO diagnostics output
C--    Contents:
C--    o DIAGNOSTICS_NF90IO_OUT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_NF90IO_OUT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_NF90IO_OUT(
     I     NrMax, nLevOutp, listId, ndId,
     I     qtmp, incrementRec, timeRec,
     I     misValLoc, myTime, myIter, myThid )

C     !DESCRIPTION:
C     write diagnostics fields to MNC file.

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
C     qtmp            :: output-field array to write
C     incrementRec    :: logical to increment record number or not
C     timeRec         :: time of start and end of averaging.
C     misValLoc       :: local Missing Value
C     myTime          :: current time of simulation (s)
C     myIter          :: current iteration number
C     myThid          :: my Thread Id number
      INTEGER NrMax
      INTEGER nLevOutp
      INTEGER listId, ndId
      _RL     qtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     misValLoc
      _RL     myTime
      INTEGER myIter, myThid
      _RL timeRec(2)
      LOGICAL incrementRec
CEOP

#ifdef ALLOW_NF90IO
C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
C     bi,bj :: tile indices
      INTEGER i, j, k
      INTEGER bi, bj

c     CHARACTER*(MAX_LEN_MBUF) msgBuf
c     INTEGER ll, llMx, jj, jjMx
      INTEGER ii, klev
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_name
      CHARACTER*(MAX_LEN_FNAM) fname
c     CHARACTER*(NLEN) dn_blnk
      LOGICAL useMisValForThisDiag
      REAL*8  misval_r8(2)
      REAL*4  misval_r4(2)
      INTEGER ncid, varid, rec_dimid, i_dimid, j_dimid, k_dimid, err
      INTEGER iLen
      INTEGER dimids(4)
      INTEGER iRec
      INTEGER misval_int(2)
      LOGICAL lexist

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     IF (useMNC .AND. diag_mnc) THEN

C        NF90IO: don't think we need this.
C        _BEGIN_MASTER( myThid )

C     get the filename
      iLen = ILNBLNK(fnames(listId))
      WRITE( fname, '(A,A3)' ) fnames(listId)(1:iLen), '.nc'
      iLen = iLen+3
      print *,fname(1:iLen)

C     TODO:
C      wjay the heck to I do with iRec?

C     Check if fname exists.  This is all we will do - we are big kids.  If the file
C     exists and its different than this version, then we should have erased it.
C     Of course I'm stupid and often forget to erase files.
      INQUIRE(file=fname(1:iLen),exist=lexist)
      IF (.NOT.lexist) THEN
         CALL NF90IO_INIT_FILE(fname(1:iLen), myThid)
C        This defines most of the dimensions and all the grid variables.
C        Basic init doesn't add timestart and timeend variables. Lets
C        do that here.
         err = nf90io_open(fname(1:iLen),ncid)
         CALL nf90ERR(err,
     $           "Open diagnostic nc file to write",myThid)

C        put in redef mode...
         err = nf90_redef(ncid)
         CALL nf90ERR(err, "Redef mode failed!",myThid)
         err = nf90_inq_dimid(ncid, 'record', rec_dimid)
         CALL nf90ERR(err, "inq record_dimid",myThid)
         err = nf90_def_var(ncid, "timestart", NF90_DOUBLE, (/ rec_dimid
     $        /),varid)
         CALL nf90ERR(err, "defining timestart variable",myThid)
         err = nf90_def_var(ncid, "timeend", NF90_DOUBLE, (/ rec_dimid
     $        /),varid)
         CALL nf90ERR(err, "defining timeend variable",myThid)
         err = nf90_enddef(ncid)
         err = nf90_close(ncid)
         CALL nf90ERR(err, "closing after initialization",myThid)
      ENDIF

C OK, open the file and get the netcdf id
      err = nf90io_open(fname(1:iLen),ncid)
C     get the variable id
      print *,"getting varid"
      print *,cdiag(ndId)
      err = nf90_inq_varid(ncid, cdiag(ndId), varid)
      IF (.NOT.(err.EQ.nf90_NoErr)) THEN
         print *,"Defining varid"
         print *,cdiag(ndId)
C       we need to define this variable
C       put in redef mode...
        err = nf90_redef(ncid)
C       we need to initialize this variable
        err = nf90_inq_dimid(ncid, 'record', rec_dimid)
        CALL nf90ERR(err, "inq record_dimid",myThid)
C       gdiag will tell us what the other two or three dimensions are
C       so here we get i_dimid and j_dimid with the proper dimensions
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
C        do the same for k_dimid (if necessary)
         IF (nLevOutp.EQ.Nr) THEN
            err = nf90_inq_dimid(ncid, 'k', k_dimid)
            CALL nf90ERR(err, "inq k_dimid",myThid)
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'M') ) THEN
               err = nf90_inq_dimid(ncid, 'k', k_dimid)
               CALL nf90ERR(err, "inq k_dimid",myThid)
            ENDIF
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'L') ) THEN
               err = nf90_inq_dimid(ncid, 'k_l', k_dimid)
               CALL nf90ERR(err, "inq k_dimid",myThid)
            ENDIF
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'U') ) THEN
               err = nf90_inq_dimid(ncid, 'k_u', k_dimid)
               CALL nf90ERR(err, "inq k_dimid",myThid)
            ENDIF

            dimids =  (/  i_dimid, j_dimid, k_dimid, rec_dimid /)
            err = nf90_def_var(ncid, cdiag(ndId), NF90_DOUBLE, dimids,
     $           varid)
         ELSEIF (nLevOutp.EQ.1) THEN
C           Define the one-level variable
            dimids(1:3) =  (/  i_dimid, j_dimid, rec_dimid /)
            err = nf90_def_var(ncid, cdiag(ndId), NF90_DOUBLE,
     $           dimids(1:3), varid)
         ELSE
C           This is a multi-level diagnostic and we need to define a new
C           dimension k_level.
            err = nf90_inq_dimid(ncid, 'k_level', k_dimid)
            IF (.NOT.(err.EQ.nf90_NoErr)) THEN
C             we need to define k_level
               err = nf90_def_dim(ncid, "k_level", nLevOutp,
     $              k_dimid)
               CALL nf90ERR(err,"Adding k_level dim",myThid)
               err = nf90_def_var(ncid, "k_level", NF90_INT, (/ k_dimid
     $              /),varid)
               CALL nf90ERR(err,"Adding k_level variable",myThid)
               err = nf90_put_att(ncid, varid,
     $              "standard_name",
     $              "z_grid_index")
               err = nf90_put_att(ncid, varid,
     $              "long_name",
     $              "z-dimension of the level grid")
C              Drop out of def mode to fill the values in here
               err = nf90_enddef(ncid)
               err = nf90io_var_par_access(ncid, varid)
               CALL nf90ERR(err, "Setting k variable to par access"
     $              ,myThid)
               err = nf90_put_var(ncid, varid, levs(1:nLevOutp, listId),
     $              start = (/ 1 /) , count = (/ nLevOutp /) )
               CALL nf90ERR(err, "putting data in  k_level variable"
     $              ,myThid)
               err = nf90_redef(ncid)
               CALL nf90ERR(err, "Put netcdf file back in redef"
     $              ,myThid)
            ENDIF
C           Done if we need to define k_level
            dimids =  (/  i_dimid, j_dimid, k_dimid, rec_dimid /)
            err = nf90_def_var(ncid, cdiag(ndId), NF90_DOUBLE, dimids,
     $           varid)
         ENDIF
         CALL nf90ERR(err, "Adding diagsnostic variable",myThid)
C        add attributes to the newy defined variable
         err = nf90_put_att(ncid, varid,
     $        "description",
     $        tdiag(ndId))
         CALL nf90ERR(err, "Adding attribute diagnostic variable"
     $        ,myThid)
         err = nf90_put_att(ncid, varid,
     $        "units",
     $        udiag(ndId))
         CALL nf90ERR(err, "Adding attribute diagnostic variable"
     $        ,myThid)
C        no standard name?  Booo
C        end definition mode...
         err = nf90_enddef(ncid)
      ENDIF
C     Done init variable.  varid should be set to receive!

C TODO: the mnc routines had a bunch of stuff setting the missing value in here.
C     Need to figure out what record number we are on and increment if we need to
      err = nf90_inq_dimid(ncid, "record", rec_dimid)
      err = nf90_inquire_dimension(ncid, rec_dimid, len=iRec)
      CALL nf90ERR(err, "Getting length of unlimited dimension",myThid)
      IF (incrementRec) THEN
        iRec = iRec+1
        err = nf90_inq_varid(ncid, "record", varid)
        CALL nf90ERR(err, "Get the record varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid, (/ iRec /), start = (/ iRec /),
     $       count = (/ 1 /) )
        CALL nf90ERR(err, "Write iRec to the record variable",myThid)
C       TIME: We should add TIMESTART and TIMEEND so that we can spec avg interval
        err = nf90_inq_varid(ncid, "time", varid)
        CALL nf90ERR(err, "Get the time varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid, (/ myTime /), start = (/ iRec /)
     $       ,count = (/ 1 /) )
        CALL nf90ERR(err, "Write time variable",myThid)
        print *,"Wrote time"

        err = nf90_inq_varid(ncid, "timestart", varid)
        CALL nf90ERR(err, "Get the timestart varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid, (/ timeRec(1) /), start = (/
     $       iRec /),count = (/ 1 /) )
        CALL nf90ERR(err, "Write timestart variable",myThid)
        err = nf90_inq_varid(ncid, "timeend", varid)
        CALL nf90ERR(err, "Get the timeend varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid, (/ timeRec(2) /), start = (/
     $       iRec /),count = (/ 1 /) )
        CALL nf90ERR(err, "Write timeend variable",myThid)
C       ITERATION
        err = nf90_inq_varid(ncid, "iter", varid)
        CALL nf90ERR(err, "Get the time varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid, (/ myIter /), start = (/ iRec /)
     $       ,count = (/ 1 /) )
        CALL nf90ERR(err, "Write iter variable",myThid)
      ENDIF

C     Write the data
      IF (nLevOutp.EQ.1) THEN
         CALL NF90IO_FILL_3D(ncid, cdiag(ndId), qtmp(:,:,1,:,:), iRec,
     $        myThid)
      ELSEIF (nLevOutp.LT.Nr) THEN
         CALL NF90IO_FILL_4DNlev(ncid, cdiag(ndId), nLevOutp,
     $        qtmp(:,:,1:nLevOutp,:,: ), iRec, myThid)

      ELSE
         CALL NF90IO_FILL_4D(ncid, cdiag(ndId), qtmp(:,:,1:Nr,:,: ),
     $        iRec, myThid)
      ENDIF

      err = nf90_close(ncid)
      CALL nf90ERR(err, "Closing netcdf file",myThid)

#endif /*  ALLOW_NF90IO  */

      RETURN
      END
