#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     !ROUTINE: EXF_SET_FLD
C     !INTERFACE:
      SUBROUTINE EXF_SET_FLD(
     I     fldName, fldFile, fldMask,
     I     fldStartTime, fldPeriod, fldRepeatCycle,
     I     fld_inScale, fldRemove_intercept, fldRemove_slope,
     U     fldArr, fld0, fld1,
#ifdef USE_EXF_INTERPOLATION
     I     fld_lon0, fld_lon_inc, fld_lat0, fld_lat_inc,
     I     fld_nlon, fld_nlat, fld_xout, fld_yout, interp_method,
#endif
     I     myTime, myIter, myThid )

C !DESCRIPTION: \bv
C  *=================================================================*
C  | SUBROUTINE EXF_SET_FLD
C  | o Set value of one generic external forcing field
C  *=================================================================*
C  |  started: Ralf.Giering@FastOpt.de 25-Mai-2000
C  |  changed: heimbach@mit.edu 10-Jan-02
C  |        20-Dec-02: mods for pkg/seaice, menemenlis@jpl.nasa.gov
C  |           heimbach@mit.edu: totally re-organized exf_set_...
C  |           replaced all routines by one generic routine
C  |        5-Aug-03: added USE_EXF_INTERPOLATION for arbitrary
C  |                    input grid capability
C  |  11-Dec-06 added time-mean and monthly-mean climatology options
C  |     fldPeriod=0 means input file is one time-constant field
C  |     fldPeriod=-12 means input file contains 12 monthly means
C  *=================================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"

C !INPUT/OUTPUT PARAMETERS:
C     fldName        :: field short name (to print mesg)
C     fldFile        :: file-name for this field
C     fldStartTime   :: corresponding starting time (in sec) for this field
C     fldPeriod      :: time period (in sec) between 2 reccords
C     fldRepeatCycle :: time duration of a repeating cycle
C     fld_inScale    :: input field scaling factor
C     fldRemove_intercept  ::
C     fldRemove_slope      ::
C     fldArr         :: field array containing current time values
C     fld0           :: field array holding previous reccord
C     fld1           :: field array holding next     reccord
#ifdef USE_EXF_INTERPOLATION
C     fld_lon0, fld_lat0   :: longitude and latitude of SouthWest
C                          :: corner of global input grid
C     fld_nlon, fld_nlat   :: input x-grid and y-grid size
C     fld_lon_inc          :: scalar x-grid increment
C     fld_lat_inc          :: vector y-grid increments
C     fld_xout, fld_yout   :: coordinates for output grid
C     fld_xout, fld_yout   :: coordinates for output grid
C     interp_method        :: select interpolation method (integer)
#endif /* USE_EXF_INTERPOLATION */
C     myTime         :: Current time (in sec) in simulation
C     myIter         :: Current iteration number
C     myThid         :: My Thread Id number
      CHARACTER*(*) fldName
      CHARACTER*(128) fldFile
      CHARACTER*1 fldMask
      _RL fldStartTime, fldPeriod, fldRepeatCycle
      _RL fld_inScale
      _RL fldRemove_intercept, fldRemove_slope
      _RL fldArr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef USE_EXF_INTERPOLATION
      _RL fld_lon0, fld_lon_inc
      _RL fld_lat0, fld_lat_inc(MAX_LAT_INC)
      INTEGER fld_nlon, fld_nlat
      _RS fld_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fld_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER interp_method
#endif /* USE_EXF_INTERPOLATION */
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL first, changed
      INTEGER count0, count1
      INTEGER year0, year1
      INTEGER bi, bj, i, j
      _RL     fac
      CHARACTER*(128) locFile0, locFile1
#ifdef USE_EXF_INTERPOLATION
      CHARACTER*(MAX_LEN_FNAM) out_file
# ifndef EXF_INTERP_USE_DYNALLOC
      _RL     bufArr( exf_interp_bufferSize )
# endif
#endif /* USE_EXF_INTERPOLATION */
CEOP

      IF ( fldFile .NE. ' ' .AND. fldPeriod .NE. 0. ) THEN

         IF ( exf_debugLev.GE.debLevD ) THEN
           _BEGIN_MASTER( myThid )
           j = ILNBLNK(fldFile)
           WRITE(msgBuf,'(5A)') 'EXF_SET_FLD: ',
     &       'processing field "', fldName, '",  file: ', fldFile(1:j)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
         ENDIF

         IF ( useCAL .AND. fldPeriod .EQ. -12. ) THEN
#ifdef ALLOW_CAL
C-    fldPeriod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
            CALL cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1, year0, year1,
     I           myTime, myIter, myThid )
#endif /* ALLOW_CAL */
         ELSEIF ( useCal .AND. fldperiod .EQ. -1.) THEN
C-    fldPeriod=-1 means fields are monthly means.
C     With useExfYearlyFields=.TRUE., each yearly input file contains
C     12 monthly mean records.  Otherwise, a single input file contains
C     monthly mean records starting at the month fldStartTime falls in.
#ifdef ALLOW_CAL
            CALL EXF_GetMonthsRec(
     I           fldStartTime, useExfYearlyFields,
     O           fac, first, changed,
     O           count0, count1, year0, year1,
     I           myTime, myIter, myThid )
#endif /* ALLOW_CAL */
         ELSEIF ( fldPeriod .LT. 0. ) THEN
           j = ILNBLNK(fldFile)
           WRITE(msgBuf,'(4A,1PE16.8,2A)') 'EXF_SET_FLD: ',
     &       '"', fldName, '", Invalid fldPeriod=', fldPeriod,
     &       ' for file: ', fldFile(1:j)
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R EXF_SET_FLD'
         ELSE
C-    get record numbers and interpolation factor for this field
            CALL EXF_GetFFieldRec(
     I           fldStartTime, fldPeriod, fldRepeatCycle,
     I           fldName, useExfYearlyFields,
     O           fac, first, changed,
     O           count0, count1, year0, year1,
     I           myTime, myIter, myThid )

         ENDIF
         IF ( exf_debugLev.GE.debLevD ) THEN
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(2A,I10,2I7)') 'EXF_SET_FLD: ',
     &       '  myIter, count0, count1:', myIter, count0, count1
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2A,2(L2,2X),F21.17)') 'EXF_SET_FLD: ',
     &       '  first, changed, fac:  ', first, changed, fac
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
         ENDIF

         IF ( first ) THEN
            CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, fldPeriod, year0,
     I         fldFile,
     O         locFile0,
     I         myTime, myIter, myThid )
            IF ( exf_debugLev.GE.debLevC ) THEN
              _BEGIN_MASTER(myThid)
              j = ILNBLNK(locFile0)
              WRITE(msgBuf,'(4A,I10,A,I6)') 'EXF_SET_FLD: ',
     &          'field "', fldName, '", it=', myIter,
     &          ', loading rec=', count0
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
              WRITE(msgBuf,'(4A)') 'EXF_SET_FLD: ',
     &          '  from file: "', locFile0(1:j), '"'
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
              _END_MASTER(myThid)
            ENDIF

#ifdef USE_EXF_INTERPOLATION
            IF ( interp_method.GE.1 ) THEN
              CALL EXF_INTERP(
     I             locFile0, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             fld1,
#else
     O             fld1, bufArr,
#endif
     I             count0, fld_xout, fld_yout,
     I             fld_lon0, fld_lon_inc, fld_lat0, fld_lat_inc,
     I             fld_nlon, fld_nlat, interp_method, myIter, myThid )

              IF ( exf_output_interp ) THEN
               j = ILNBLNK(locFile0)
               WRITE(out_file,'(2A)') locFile0(1:j), '_out'
               IF ( count0.NE.1 )
     &         CALL WRITE_REC_XY_RL( out_file, fld1, 1,
     &                                         myIter, myThid )
               CALL WRITE_REC_XY_RL( out_file, fld1, count0,
     &                                         myIter, myThid )
              ENDIF

            ELSE
#endif /* USE_EXF_INTERPOLATION */
              CALL READ_REC_3D_RL( locFile0, exf_iprec, 1,
     &                             fld1, count0, myIter, myThid )
#ifdef USE_EXF_INTERPOLATION
            ENDIF
#endif /* USE_EXF_INTERPOLATION */

C-    apply mask
            CALL EXF_FILTER_RL( fld1, fldMask, myThid )

C-    end if ( first ) block
         ENDIF

         IF ( first .OR. changed ) THEN
            CALL exf_SwapFFields( fld0, fld1, myThid )

            CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, fldPeriod, year1,
     I         fldFile,
     O         locFile1,
     I         myTime, myIter, myThid )
            IF ( exf_debugLev.GE.debLevC ) THEN
              _BEGIN_MASTER(myThid)
              j = ILNBLNK(locFile1)
              WRITE(msgBuf,'(4A,I10,A,I6)') 'EXF_SET_FLD: ',
     &          'field "', fldName, '", it=', myIter,
     &          ', loading rec=', count1
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
              WRITE(msgBuf,'(4A)') 'EXF_SET_FLD: ',
     &          '  from file: "', locFile1(1:j), '"'
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
              _END_MASTER(myThid)
            ENDIF

#ifdef USE_EXF_INTERPOLATION
            IF ( interp_method.GE.1 ) THEN
              CALL EXF_INTERP(
     I             locFile1, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             fld1,
#else
     O             fld1, bufArr,
#endif
     I             count1, fld_xout, fld_yout,
     I             fld_lon0, fld_lon_inc, fld_lat0, fld_lat_inc,
     I             fld_nlon, fld_nlat, interp_method, myIter, myThid )

              IF ( exf_output_interp ) THEN
               j = ILNBLNK(locFile1)
               WRITE(out_file,'(2A)') locFile1(1:j), '_out'
               CALL WRITE_REC_XY_RL( out_file, fld1, count1,
     &                                         myIter, myThid )
              ENDIF

            ELSE
#endif /* USE_EXF_INTERPOLATION */
              CALL READ_REC_3D_RL( locFile1, exf_iprec, 1,
     &                             fld1, count1, myIter, myThid )
#ifdef USE_EXF_INTERPOLATION
            ENDIF
#endif /* USE_EXF_INTERPOLATION */

C-    apply mask
            CALL EXF_FILTER_RL( fld1, fldMask, myThid )

C-    end if ( first or changed ) block
         ENDIF

C     Loop over tiles.
         DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
C     Interpolate linearly onto the  time.
             fldArr(i,j,bi,bj) =     fld_inScale * (
     &                       fac * fld0(i,j,bi,bj)
     &          + (exf_one - fac)* fld1(i,j,bi,bj) )
             fldArr(i,j,bi,bj) = fldArr(i,j,bi,bj)
     &         - fld_inScale*( fldRemove_intercept
     &                         + fldRemove_slope*(myTime-startTime) )
            ENDDO
           ENDDO
          ENDDO
         ENDDO

      ENDIF

      RETURN
      END
