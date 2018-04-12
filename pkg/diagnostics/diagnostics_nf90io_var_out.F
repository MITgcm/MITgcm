#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_NF90IO_VAR_OUT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_NF90IO_VAR_OUT(
     I     ncid,
     I     NrMax, nLevOutp, listId, ndId,
     I     qtmp, incrementRec, timeRec,
     I     misValLoc, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write diagnostics field to parallel netCDF file.

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
      INTEGER ncid
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

C     !LOCAL VARIABLES:
      INTEGER varid, rec_dimid, i_dimid, j_dimid, k_dimid, err
      INTEGER iRec

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     IF (useMNC .AND. diag_mnc) THEN

      _BEGIN_MASTER( myThid )

C     get the variable id
      err = nf90_inq_varid(ncid, cdiag(ndId), varid)
      CALL nf90ERR(err, "Get variable varid", myThid)

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

      _END_MASTER( myThid )

#endif /*  ALLOW_NF90IO  */

      RETURN
      END
