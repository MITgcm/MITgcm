#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_INIT_FLD
C     !INTERFACE:
      SUBROUTINE EXF_INIT_FLD (
     I     fldName, fldFile, fldMask,
     I     fldPeriod, fld_inScale, fldConst,
     U     fldArr, fld0, fld1,
#ifdef USE_EXF_INTERPOLATION
     I     fld_lon0, fld_lon_inc, fld_lat0, fld_lat_inc,
     I     fld_nlon, fld_nlat, fld_xout, fld_yout, interp_method,
#endif
     I     myThid )

C !DESCRIPTION: \bv
C  *=================================================================*
C  | SUBROUTINE EXF_INIT_FLD
C  *=================================================================*
C  |  started: Ralf.Giering@FastOpt.de 25-Mai-2000
C  |  changed: heimbach@mit.edu 10-Jan-2002
C  |           heimbach@mit.edu: totally re-organized exf_set_...
C  |           replaced all routines by one generic routine
C  *=================================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "EXF_PARAM.h"
#include "EXF_INTERP_SIZE.h"

C !INPUT/OUTPUT PARAMETERS:
C     fldName        :: field short name (to print mesg)
C     fldFile        :: file-name for this field
C     fldPeriod      :: time period (in sec) between 2 reccords
C     fld_inScale    :: input field scaling factor
C     fldConst       :: uniform default field value
C     fldArr         :: field array containing current time values
C     fld0           :: field array holding previous reccord
C     fld1           :: field array holding next     reccord
C     myThid         :: My Thread Id number
      CHARACTER*(*) fldName
      CHARACTER*(128) fldFile
      CHARACTER*1 fldMask
      _RL fldPeriod, fld_inScale, fldConst
      _RL fldArr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

#ifdef USE_EXF_INTERPOLATION
C     fld_lon0, fld_lat0   :: longitude and latitude of SouthWest
C                             corner of global input grid
C     fld_nlon, fld_nlat   :: input x-grid and y-grid size
C     fld_lon_inc          :: scalar x-grid increment
C     fld_lat_inc          :: vector y-grid increments
C     fld_xout, fld_yout   :: coordinates for output grid
      _RL fld_lon0, fld_lon_inc
      _RL fld_lat0, fld_lat_inc(MAX_LAT_INC)
      INTEGER fld_nlon, fld_nlat
      _RS fld_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fld_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER interp_method
#endif /* USE_EXF_INTERPOLATION */

C !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, i, j, count
#ifdef USE_EXF_INTERPOLATION
# ifndef EXF_INTERP_USE_DYNALLOC
      _RL     bufArr( exf_interp_bufferSize )
# endif
#endif /* USE_EXF_INTERPOLATION */
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy, sNy+OLy
            DO i = 1-OLx, sNx+OLx
              fldArr(i,j,bi,bj)  = fldConst
              fld0(i,j,bi,bj)    = fldConst
              fld1(i,j,bi,bj)    = fldConst
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF ( fldFile .NE. ' ' .AND. fldPeriod .EQ. 0. ) THEN
         count = 1
         IF ( exf_debugLev.GE.debLevC ) THEN
           _BEGIN_MASTER(myThid)
           j = ILNBLNK(fldFile)
           WRITE(msgBuf,'(4A,I3,2A)') 'EXF_INIT_FLD: ',
     &         'field "', fldName,
     &         '", loading rec=', count, ' from: ', fldFile(1:j)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           _END_MASTER(myThid)
         ENDIF

#ifdef USE_EXF_INTERPOLATION
         IF ( interp_method.GE.1 ) THEN
              CALL EXF_INTERP(
     I             fldFile, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             fldArr,
#else
     O             fldArr, bufArr,
#endif
     I             count, fld_xout, fld_yout,
     I             fld_lon0, fld_lon_inc, fld_lat0, fld_lat_inc,
     I             fld_nlon, fld_nlat, interp_method, 0, myThid )
         ELSE
#endif /* USE_EXF_INTERPOLATION */
            CALL READ_REC_3D_RL( fldFile, exf_iprec, 1,
     &                           fldArr, count, 0, myThid )
#ifdef USE_EXF_INTERPOLATION
         ENDIF
#endif /* USE_EXF_INTERPOLATION */

C-    apply mask
         CALL EXF_FILTER_RL( fldArr, fldMask, myThid )

C     Loop over tiles and scale fldArr
         DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              fldArr(i,j,bi,bj) = fld_inScale*fldArr(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

      ENDIF

      RETURN
      END
