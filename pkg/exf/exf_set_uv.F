#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     !ROUTINE: EXF_SET_UV
C     !INTERFACE:
      SUBROUTINE EXF_SET_UV(
     I     uVecName, uVecFile, uVecMask,
     I     uVecStartTime, uVecPeriod, uVecRepeatCycle,
     I     uVec_inScale, uVec_remove_intercept, uVec_remove_slope,
     U     uVec, uVec0, uVec1,
     I     vVecName, vVecFile, vVecMask,
     I     vVecStartTime, vVecPeriod, vVecRepeatCycle,
     I     vVec_inScale, vVec_remove_intercept, vVec_remove_slope,
     U     vVec, vVec0, vVec1,
#ifdef USE_EXF_INTERPOLATION
     I     uVec_lon0, uVec_lon_inc, uVec_lat0, uVec_lat_inc,
     I     uVec_nlon, uVec_nlat, u_interp_method,
     I     vVec_lon0, vVec_lon_inc, vVec_lat0, vVec_lat_inc,
     I     vVec_nlon, vVec_nlat, v_interp_method, uvInterp,
#endif /* USE_EXF_INTERPOLATION */
     I     myTime, myIter, myThid )

C !DESCRIPTION: \bv
C  *=================================================================*
C  | SUBROUTINE EXF_SET_UV
C  | o Read-in, interpolate, and rotate wind or wind stress vectors
C  |   from a spherical-polar input grid to an arbitrary output grid.
C  *=================================================================*
C  |   menemenlis@jpl.nasa.gov, 8-Dec-2003
C  *=================================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"
#include "EXF_FIELDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     *VecName        :: vector compon. short name (to print mesg)
C     *VecFile        :: file-name for this vector compon. field
C     *VecStartTime   :: corresponding starting time (in sec) for this vec
C     *VecPeriod      :: time period (in sec) between 2 reccords
C     *VecRepeatCycle :: time duration of a repeating cycle
C     *Vec_inScale    :: input field scaling factor
C     *VecRemove_intercept ::
C     *VecRemove_slope     ::
C     *Vec            :: field array containing current time values
C     *Vec0           :: field array holding previous reccord
C     *Vec1           :: field array holding next     reccord
#ifdef USE_EXF_INTERPOLATION
C     *vec_lon0, *vec_lat0 :: longitude and latitude of SouthWest
C                          :: corner of global input grid for *vec
C     *vec_nlon, *vec_nlat :: input x-grid and y-grid size for *vec
C     *_interp_method      :: select interpolation method for *vec
C     *vec_lon_inc         :: scalar x-grid increment for *vec
C     *vec_lat_inc         :: vector y-grid increments for *vec
#endif /* USE_EXF_INTERPOLATION */
C     myTime         :: Current time (in sec) in simulation
C     myIter         :: Current iteration number
C     myThid         :: My Thread Id number
      CHARACTER*(*) uVecName
      CHARACTER*(128) uVecFile
      CHARACTER*1 uVecMask
      _RL     uVecStartTime, uVecPeriod, uVecRepeatCycle
      _RL     uVec_inScale
      _RL     uVec_remove_intercept, uVec_remove_slope
      _RL     uVec  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     uVec0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     uVec1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(*) vVecName
      CHARACTER*(128) vVecFile
      CHARACTER*1 vVecMask
      _RL     vVecStartTime, vVecPeriod, vVecRepeatCycle
      _RL     vVec_inScale
      _RL     vVec_remove_intercept, vVec_remove_slope
      _RL     vVec  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     vVec0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     vVec1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef USE_EXF_INTERPOLATION
      _RL uVec_lon0, uVec_lon_inc
      _RL uVec_lat0, uVec_lat_inc(MAX_LAT_INC)
      INTEGER uVec_nlon, uVec_nlat, u_interp_method
      _RL vVec_lon0, vVec_lon_inc
      _RL vVec_lat0, vVec_lat_inc(MAX_LAT_INC)
      INTEGER vVec_nlon, vVec_nlat, v_interp_method
      LOGICAL uvInterp
#endif /* USE_EXF_INTERPOLATION */
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      _RL     tmp_u (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     tmp_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef USE_EXF_INTERPOLATION
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(128) uVecFile0, uVecFile1
      CHARACTER*(128) vVecFile0, vVecFile1
      CHARACTER*(MAX_LEN_FNAM) out_uVecFile, out_vVecFile
      LOGICAL first, changed
      _RL     fac
#ifdef EXF_USE_OLD_VEC_ROTATION
      _RL     x1, x2, x3, x4, y1, y2, y3, y4, dx, dy
#endif
      INTEGER count0, count1
      INTEGER year0, year1
# ifndef EXF_INTERP_USE_DYNALLOC
      _RL     bufArr1( exf_interp_bufferSize )
      _RL     bufArr2( exf_interp_bufferSize )
# endif
#endif /* USE_EXF_INTERPOLATION */
CEOP

#ifdef USE_EXF_INTERPOLATION
      IF ( u_interp_method.GE.1 .AND. v_interp_method.GE.1 .AND.
     &     uVecFile.NE.' ' .AND. vVecFile.NE.' ' .AND.
     &     (usingCurvilinearGrid .OR. rotateGrid .OR. uvInterp) ) THEN

        IF ( exf_debugLev.GE.debLevD ) THEN
          _BEGIN_MASTER( myThid )
           i = ILNBLNK(uVecFile)
           j = ILNBLNK(vVecFile)
           WRITE(msgBuf,'(6A)') 'EXF_SET_UV: ',
     &       'processing fields "', uVecName, '" & "', vVecName, '"'
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(6A)') 'EXF_SET_UV: ',
     &       '  files: ', uVecFile(1:i), ' & ', vVecFile(1:j)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          _END_MASTER( myThid )
        ENDIF
        IF ( useCAL .AND. uVecPeriod .EQ. -12. ) THEN
#ifdef ALLOW_CAL
C-    genperiod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
          CALL cal_GetMonthsRec(
     O             fac, first, changed,
     O             count0, count1, year0, year1,
     I             myTime, myIter, myThid )
#endif /* ALLOW_CAL */
        ELSEIF ( useCAL .AND. uVecPeriod .EQ. -1.) THEN
#ifdef ALLOW_CAL
C-    uVecPeriod=-1 means fields are monthly means.
C     With useExfYearlyFields=.TRUE., each yearly input file contains
C     12 monthly mean records.  Otherwise, a single input file contains
C     monthly mean records starting at the month uVecStartTime falls in.
          CALL EXF_GetMonthsRec(
     I             uVecStartTime, useExfYearlyFields,
     O             fac, first, changed,
     O             count0, count1, year0, year1,
     I             myTime, myIter, myThid )
#endif /* ALLOW_CAL */
        ELSEIF ( uVecPeriod .LT. 0. ) THEN
          j = ILNBLNK(uVecFile)
          WRITE(msgBuf,'(4A,1PE16.8,2A)') 'EXF_SET_UV: ',
     &      '"', uVecName, '", Invalid uVecPeriod=', uVecPeriod,
     &      ' for file: ', uVecFile(1:j)
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R EXF_SET_UV'
        ELSE
C-    get record numbers and interpolation factor
          CALL EXF_GetFFieldRec(
     I             uVecStartTime, uVecPeriod, uVecRepeatCycle,
     I             uVecName, useExfYearlyFields,
     O             fac, first, changed,
     O             count0, count1, year0, year1,
     I             myTime, myIter, myThid )
        ENDIF
        IF ( exf_debugLev.GE.debLevD ) THEN
          _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(2A,I10,2I7)') 'EXF_SET_UV:  ',
     &       ' myIter, count0, count1:', myIter, count0, count1
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2A,2(L2,2X),F21.17)') 'EXF_SET_UV:  ',
     &       ' first, changed, fac:  ', first, changed, fac
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          _END_MASTER( myThid )
        ENDIF

        IF ( first ) THEN
C--   Load and interpolate a new reccord (= 1rst one of this run)

          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, uVecPeriod, year0,
     I         uVecFile,
     O         uVecFile0,
     I         myTime, myIter, myThid )
          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, vVecPeriod, year0,
     I         vVecFile,
     O         vVecFile0,
     I         myTime, myIter, myThid )
          IF ( exf_debugLev.GE.debLevC ) THEN
            _BEGIN_MASTER(myThid)
            WRITE(msgBuf,'(6A,I10)') 'EXF_SET_UV: ',
     &        'fields "', uVecName, '" & "', vVecName, '", it=', myIter
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            j = ILNBLNK(uVecFile0)
            WRITE(msgBuf,'(2A,I6,3A)') 'EXF_SET_UV:   ',
     &      'loading rec=', count0, ' from file: "', uVecFile0(1:j), '"'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            j = ILNBLNK(vVecFile0)
            WRITE(msgBuf,'(2A,I6,3A)') 'EXF_SET_UV:   ',
     &      'loading rec=', count0, ' from file: "', vVecFile0(1:j), '"'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            _END_MASTER(myThid)
          ENDIF

          IF ( uvInterp ) THEN
C-    vector interpolation to (xC,yC) locations
           CALL EXF_INTERP_UV(
     I             uVecFile0, vVecFile0, exf_iprec, count0,
     I             uVec_nlon, uVec_nlat,
     I             uVec_lon0, uVec_lon_inc, uVec_lat0, uVec_lat_inc,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             tmp_u, tmp_v,
#else
     O             tmp_u, tmp_v, bufArr1, bufArr2,
#endif
     I             xC, yC,
     I             u_interp_method, v_interp_method, myIter, myThid )
          ELSE
C-    scalar interpolation to (xC,yC) locations
           CALL EXF_INTERP(
     I             uVecFile0, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             tmp_u,
#else
     O             tmp_u, bufArr1,
#endif
     I             count0, xC, yC,
     I             uVec_lon0, uVec_lon_inc, uVec_lat0, uVec_lat_inc,
     I             uVec_nlon, uVec_nlat, u_interp_method,
     I             myIter, myThid )
           CALL EXF_INTERP(
     I             vVecFile0, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             tmp_v,
#else
     O             tmp_v, bufArr2,
#endif
     I             count0, xC, yC,
     I             vVec_lon0, vVec_lon_inc, vVec_lat0, vVec_lat_inc,
     I             vVec_nlon, vVec_nlat, v_interp_method,
     I             myIter, myThid )
          ENDIF

C-    apply mask: Note: done after applying scaling factor and rotation
c         CALL EXF_FILTER_RL( tmp_u, uVecMask, myThid )
c         CALL EXF_FILTER_RL( tmp_v, vVecMask, myThid )

          IF ( exf_output_interp ) THEN
           j = ILNBLNK(uVecFile0)
           WRITE(out_uVecFile,'(2A)') uVecFile0(1:j), '_out'
           IF ( count0.NE.1 )
     &     CALL WRITE_REC_XY_RL(out_uVecFile,tmp_u,1,myIter,myThid)
           CALL WRITE_REC_XY_RL(out_uVecFile,tmp_u,count0,myIter,myThid)
           j = ILNBLNK(vVecFile0)
           WRITE(out_vVecFile,'(2A)') vVecFile0(1:j), '_out'
           IF ( count0.NE.1 )
     &     CALL WRITE_REC_XY_RL(out_vVecFile,tmp_v,1,myIter,myThid)
           CALL WRITE_REC_XY_RL(out_vVecFile,tmp_v,count0,myIter,myThid)
          ENDIF

C-    scaling factor and vector rotation
          IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
            DO bj = myByLo(myThid),myByHi(myThid)
             DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                 tmp_u(i,j,bi,bj) = uVec_inScale*tmp_u(i,j,bi,bj)
                 tmp_v(i,j,bi,bj) = vVec_inScale*tmp_v(i,j,bi,bj)
               ENDDO
              ENDDO
              DO j = 1,sNy
               DO i = 1,sNx
#ifdef EXF_USE_OLD_VEC_ROTATION
                    x1=xG(i,j,bi,bj)
                    x2=xG(i+1,j,bi,bj)
                    x3=xG(i,j+1,bi,bj)
                    x4=xG(i+1,j+1,bi,bj)
                    IF ((x2-x1).GT.180) x2=x2-360
                    IF ((x1-x2).GT.180) x2=x2+360
                    IF ((x3-x1).GT.180) x3=x3-360
                    IF ((x1-x3).GT.180) x3=x3+360
                    IF ((x4-x1).GT.180) x4=x4-360
                    IF ((x1-x4).GT.180) x4=x4+360
                    y1=yG(i,j,bi,bj)
                    y2=yG(i+1,j,bi,bj)
                    y3=yG(i,j+1,bi,bj)
                    y4=yG(i+1,j+1,bi,bj)
                    dx=0.5*(x3+x4-x1-x2)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y3+y4-y1-y2)
                    vVec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
                    dx=0.5*(x2+x4-x1-x3)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y2+y4-y1-y3)
                    uVec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
#else /* EXF_USE_OLD_VEC_ROTATION */
                 uVec1(i,j,bi,bj) =
     &                      angleCosC(i,j,bi,bj)*tmp_u(i,j,bi,bj)
     &                     +angleSinC(i,j,bi,bj)*tmp_v(i,j,bi,bj)
                 vVec1(i,j,bi,bj) =
     &                     -angleSinC(i,j,bi,bj)*tmp_u(i,j,bi,bj)
     &                     +angleCosC(i,j,bi,bj)*tmp_v(i,j,bi,bj)
#endif /* EXF_USE_OLD_VEC_ROTATION */
               ENDDO
              ENDDO
             ENDDO
            ENDDO
          ELSE
            DO bj = myByLo(myThid),myByHi(myThid)
             DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                 uVec1(i,j,bi,bj) = uVec_inScale*tmp_u(i,j,bi,bj)
                 vVec1(i,j,bi,bj) = vVec_inScale*tmp_v(i,j,bi,bj)
               ENDDO
              ENDDO
             ENDDO
            ENDDO
          ENDIF
C-    apply mask (after scaling factor and rotation)
          CALL EXF_FILTER_RL( uVec1, uVecMask, myThid )
          CALL EXF_FILTER_RL( vVec1, vVecMask, myThid )

C-    end if ( first ) block
        ENDIF

        IF (  first .OR. changed ) THEN
C--   Load and interpolate a new reccord

          CALL exf_SwapFFields( uVec0, uVec1, myThid )
          CALL exf_SwapFFields( vVec0, vVec1, myThid )

          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, uVecPeriod, year1,
     I         uVecFile,
     O         uVecFile1,
     I         myTime, myIter, myThid )
          CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, vVecPeriod, year1,
     I         vVecFile,
     O         vVecFile1,
     I         myTime, myIter, myThid )
          IF ( exf_debugLev.GE.debLevC ) THEN
            _BEGIN_MASTER(myThid)
            WRITE(msgBuf,'(6A,I10)') 'EXF_SET_UV: ',
     &        'fields "', uVecName, '" & "', vVecName, '", it=', myIter
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            j = ILNBLNK(uVecFile1)
            WRITE(msgBuf,'(2A,I6,3A)') 'EXF_SET_UV:   ',
     &      'loading rec=', count1, ' from file: "', uVecFile1(1:j), '"'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            j = ILNBLNK(vVecFile1)
            WRITE(msgBuf,'(2A,I6,3A)') 'EXF_SET_UV:   ',
     &      'loading rec=', count1, ' from file: "', vVecFile1(1:j), '"'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            _END_MASTER(myThid)
          ENDIF

          IF ( uvInterp ) THEN
C-    vector interpolation to (xC,yC) locations
           CALL EXF_INTERP_UV(
     I             uVecFile1, vVecFile1, exf_iprec, count1,
     I             uVec_nlon, uVec_nlat,
     I             uVec_lon0, uVec_lon_inc, uVec_lat0, uVec_lat_inc,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             tmp_u, tmp_v,
#else
     O             tmp_u, tmp_v, bufArr1, bufArr2,
#endif
     I             xC, yC,
     I             u_interp_method, v_interp_method, myIter, myThid )
          ELSE
C-    scalar interpolation to (xC,yC) locations
          CALL EXF_INTERP(
     I             uVecFile1, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             tmp_u,
#else
     O             tmp_u, bufArr1,
#endif
     I             count1, xC, yC,
     I             uVec_lon0, uVec_lon_inc, uVec_lat0, uVec_lat_inc,
     I             uVec_nlon, uVec_nlat, u_interp_method,
     I             myIter, myThid )
          CALL EXF_INTERP(
     I             vVecFile1, exf_iprec,
#ifdef EXF_INTERP_USE_DYNALLOC
     O             tmp_v,
#else
     O             tmp_v, bufArr2,
#endif
     I             count1, xC, yC,
     I             vVec_lon0, vVec_lon_inc, vVec_lat0, vVec_lat_inc,
     I             vVec_nlon, vVec_nlat, v_interp_method,
     I             myIter, myThid )
          ENDIF

C-    apply mask: Note: done after applying scaling factor and rotation
c         CALL EXF_FILTER_RL( tmp_u, uVecMask, myThid )
c         CALL EXF_FILTER_RL( tmp_v, vVecMask, myThid )

          IF ( exf_output_interp ) THEN
           j = ILNBLNK(uVecFile1)
           WRITE(out_uVecFile,'(2A)') uVecFile1(1:j), '_out'
           CALL WRITE_REC_XY_RL(out_uVecFile,tmp_u,count1,myIter,myThid)
           j = ILNBLNK(vVecFile1)
           WRITE(out_vVecFile,'(2A)') vVecFile1(1:j), '_out'
           CALL WRITE_REC_XY_RL(out_vVecFile,tmp_v,count1,myIter,myThid)
          ENDIF

C-    scaling factor and vector rotation
          IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
            DO bj = myByLo(myThid),myByHi(myThid)
             DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                 tmp_u(i,j,bi,bj) = uVec_inScale*tmp_u(i,j,bi,bj)
                 tmp_v(i,j,bi,bj) = vVec_inScale*tmp_v(i,j,bi,bj)
               ENDDO
              ENDDO
              DO j = 1,sNy
               DO i = 1,sNx
#ifdef EXF_USE_OLD_VEC_ROTATION
                    x1=xG(i,j,bi,bj)
                    x2=xG(i+1,j,bi,bj)
                    x3=xG(i,j+1,bi,bj)
                    x4=xG(i+1,j+1,bi,bj)
                    IF ((x2-x1).GT.180) x2=x2-360
                    IF ((x1-x2).GT.180) x2=x2+360
                    IF ((x3-x1).GT.180) x3=x3-360
                    IF ((x1-x3).GT.180) x3=x3+360
                    IF ((x4-x1).GT.180) x4=x4-360
                    IF ((x1-x4).GT.180) x4=x4+360
                    y1=yG(i,j,bi,bj)
                    y2=yG(i+1,j,bi,bj)
                    y3=yG(i,j+1,bi,bj)
                    y4=yG(i+1,j+1,bi,bj)
                    dx=0.5*(x3+x4-x1-x2)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y3+y4-y1-y2)
                    vVec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
                    dx=0.5*(x2+x4-x1-x3)
                    dx=dx*
     &                  cos(deg2rad*yC(i,j,bi,bj))
                    dy=0.5*(y2+y4-y1-y3)
                    uVec1(i,j,bi,bj)=
     &                  (tmp_u(i,j,bi,bj)*dx+
     &                  tmp_v(i,j,bi,bj)*dy)/
     &                  SQRT(dx*dx+dy*dy)
#else /* EXF_USE_OLD_VEC_ROTATION */
                 uVec1(i,j,bi,bj) =
     &                      angleCosC(i,j,bi,bj)*tmp_u(i,j,bi,bj)
     &                     +angleSinC(i,j,bi,bj)*tmp_v(i,j,bi,bj)
                 vVec1(i,j,bi,bj) =
     &                     -angleSinC(i,j,bi,bj)*tmp_u(i,j,bi,bj)
     &                     +angleCosC(i,j,bi,bj)*tmp_v(i,j,bi,bj)
#endif /* EXF_USE_OLD_VEC_ROTATION */
               ENDDO
              ENDDO
             ENDDO
            ENDDO
          ELSE
            DO bj = myByLo(myThid),myByHi(myThid)
             DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                 uVec1(i,j,bi,bj) = uVec_inScale*tmp_u(i,j,bi,bj)
                 vVec1(i,j,bi,bj) = vVec_inScale*tmp_v(i,j,bi,bj)
               ENDDO
              ENDDO
             ENDDO
            ENDDO
          ENDIF
C-    apply mask (after scaling factor and rotation)
          CALL EXF_FILTER_RL( uVec1, uVecMask, myThid )
          CALL EXF_FILTER_RL( vVec1, vVecMask, myThid )

C-    end if ( first or changed ) block
        ENDIF

C--   Interpolate linearly onto the current time.
        DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
            DO j = 1,sNy
              DO i = 1,sNx
                uVec(i,j,bi,bj) = fac * uVec0(i,j,bi,bj)
     &               + (exf_one - fac)* uVec1(i,j,bi,bj)
                vVec(i,j,bi,bj) = fac * vVec0(i,j,bi,bj)
     &               + (exf_one - fac)* vVec1(i,j,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

      ELSE
C     case no-interpolation
C     or ( .NOT.usingCurvilinearGrid & .NOT.rotateGrid & .NOT.uvInterp )
#else  /* USE_EXF_INTERPOLATION */
      IF ( .TRUE. ) THEN
#endif /* USE_EXF_INTERPOLATION */

        CALL EXF_SET_FLD(
     I      uVecName, uVecFile, uVecMask,
     I      uVecStartTime, uVecPeriod, uVecRepeatCycle,
     I      uVec_inScale, uVec_remove_intercept, uVec_remove_slope,
     U      uVec, uVec0, uVec1,
#ifdef USE_EXF_INTERPOLATION
     I      uVec_lon0, uVec_lon_inc, uVec_lat0, uVec_lat_inc,
     I      uVec_nlon, uVec_nlat, xC, yC, u_interp_method,
#endif /* USE_EXF_INTERPOLATION */
     I      myTime, myIter, myThid )

        CALL EXF_SET_FLD(
     I      vVecName, vVecFile, vVecMask,
     I      vVecStartTime, vVecPeriod, vVecRepeatCycle,
     I      vVec_inScale, vVec_remove_intercept, vVec_remove_slope,
     U      vVec, vVec0, vVec1,
#ifdef USE_EXF_INTERPOLATION
     I      vVec_lon0, vVec_lon_inc, vVec_lat0, vVec_lat_inc,
     I      vVec_nlon, vVec_nlat, xC, yC, v_interp_method,
#endif /* USE_EXF_INTERPOLATION */
     I      myTime, myIter, myThid )

C-    vector rotation
          IF ( rotateStressOnAgrid ) THEN
            DO bj = myByLo(myThid),myByHi(myThid)
             DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                 tmp_u(i,j,bi,bj) = uVec(i,j,bi,bj)
                 tmp_v(i,j,bi,bj) = vVec(i,j,bi,bj)
               ENDDO
              ENDDO
             ENDDO
            ENDDO
            DO bj = myByLo(myThid),myByHi(myThid)
             DO bi = myBxLo(myThid),myBxHi(myThid)
              DO j = 1,sNy
               DO i = 1,sNx
                 uVec(i,j,bi,bj) =
     &                      angleCosC(i,j,bi,bj)*tmp_u(i,j,bi,bj)
     &                     +angleSinC(i,j,bi,bj)*tmp_v(i,j,bi,bj)
                 vVec(i,j,bi,bj) =
     &                     -angleSinC(i,j,bi,bj)*tmp_u(i,j,bi,bj)
     &                     +angleCosC(i,j,bi,bj)*tmp_v(i,j,bi,bj)
               ENDDO
              ENDDO
             ENDDO
            ENDDO
          ENDIF

      ENDIF

      RETURN
      END
