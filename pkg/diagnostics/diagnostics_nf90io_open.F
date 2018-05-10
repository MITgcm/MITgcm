#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_NF90IO_OPEN

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_NF90IO_OPEN(
     I                       listId,
     O                       ncid,
     I                       myTime, myIter, myThid )

C     !DESCRIPTION:
C     Open netCDF file for list listId.

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
#endif

C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     myIter  :: current iteration number
C     myTime  :: current time of simulation (s)
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER listId, myIter, myThid

C     !OUTPUT PARAMETERS:
C     ncid    :: file id of netCDF file opened
      INTEGER ncid
CEOP

#ifdef ALLOW_NF90IO

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     lm    :: loop index (averageCycle)
C     md    :: field number in the list "listId".
C     ndId  :: diagnostics  Id number (in available diagnostics list)
C     nLevOutp :: number of levels to write in output file

      INTEGER lm
      INTEGER md, ndId
      CHARACTER*10 gcode
      INTEGER nLevOutp

      INTEGER varid, rec_dimid, i_dimid, j_dimid, k_dimid, err
      INTEGER iLen
      CHARACTER*(MAX_LEN_FNAM) fname
      INTEGER prec

      LOGICAL lexist

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (useNF90io .AND. diag_nf90io) THEN

C-     for now, if integrate vertically, output field has just 1 level:
       nLevOutp = nlevels(listId)
       IF ( fflags(listId)(2:2).EQ.'I' ) THEN
         nLevOutp = 1
       ENDIF

C      get the filename
       iLen = ILNBLNK(fnames(listId))
       WRITE( fname, '(A,A3)' ) fnames(listId)(1:iLen), '.nc'
       iLen = iLen+3

C      Check if fname exists.  Assume an existing file has same
C      dimensions and variables as we need.
       INQUIRE(file=fname(1:iLen),exist=lexist)
       IF (.NOT.lexist) THEN

         CALL NF90IO_CREATE_FILE(fname(1:iLen), ncid, myThid)

C        This defines most of the dimensions and all the grid variables.
C        Basic init doesn't add timestart and timeend variables. Lets
C        do that here.

         err = nf90_inq_dimid(ncid, 'record', rec_dimid)
         CALL nf90ERR(err, "inq record_dimid",myThid)
         err = nf90_def_var(ncid, "timestart", NF90_DOUBLE, (/ rec_dimid
     $        /),varid)
         CALL nf90ERR(err, "defining timestart variable",myThid)
         err = nf90_def_var(ncid, "timeend", NF90_DOUBLE, (/ rec_dimid
     $        /),varid)
         CALL nf90ERR(err, "defining timeend variable",myThid)

C--      Place the loop on lm (= averagePeriod) outside the loop on md (= field):
         DO lm=1,averageCycle(listId)

          DO md = 1,nfields(listId)
           ndId = ABS(jdiag(md,listId))
           gcode = gdiag(ndId)(1:10)
           IF ( idiag(md,listId).NE.0 .AND. gcode(5:5).NE.'D' ) THEN
            IF ( debugLevel.GE.debLevB .AND. myThid.EQ.1 ) THEN
              WRITE(standardMessageUnit,'(A,I6,X,I6)')
     &           ' nf90io var def',listId,ndId
            ENDIF
            CALL DIAGNOSTICS_NF90IO_VAR_DEF(
     I                 ncid, nLevOutp, listId, ndId,
     I                 myTime, myIter, myThid )

C--        end of Processing Fld # md
           ENDIF
          ENDDO

C--      end loop on lm counter (= averagePeriod)
         ENDDO

C        end definition mode...
         err = nf90_enddef(ncid)
         CALL nf90ERR(err, "enddef", myThid)

C        see whether we have to fill k_level
         _BEGIN_MASTER( myThid )

         IF (nLevOutp.NE.Nr .AND. nLevOutp.NE.1) THEN
C           get the variable id
            err = nf90_inq_varid(ncid, "k_level", varid)
            CALL nf90ERR(err, "Get variable k_level varid", myThid)
            err = nf90io_var_par_access(ncid, varid)
            CALL nf90ERR(err, "Setting k variable to par access"
     &           ,myThid)
            err = nf90_put_var(ncid, varid, levs(1:nLevOutp, listId),
     &           start = (/ 1 /) , count = (/ nLevOutp /) )
            CALL nf90ERR(err, "putting data in  k_level variable"
     &           ,myThid)
         ENDIF

         _END_MASTER(myThid)

         IF ( debugLevel.GE.debLevB .AND. myThid.EQ.1 ) THEN
           WRITE(standardMessageUnit,'(A,I6,X,I6)')
     &        'NF90IO Info: fill nonrec', ncid
         ENDIF
         CALL NF90IO_FILL_NONREC( ncid, myThid )

       ELSE

         err = nf90io_open(fname(1:iLen),ncid)
         CALL nf90ERR(err, "Open diagnostic nc file to write",myThid)

       ENDIF

C     nf90io
      ENDIF

#endif /*  ALLOW_NF90IO  */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
