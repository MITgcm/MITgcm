C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_set_gen.F,v 1.32 2014/06/07 17:50:34 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C--  File exf_set_gen.F: General routines to load-in an input field
C--   Contents
C--   o EXF_SET_GEN
C--   o EXF_INIT_GEN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SET_GEN(
     &     genfile, genstartdate, genperiod,
     &     exf_inscal_gen, genremove_intercept, genremove_slope,
     &     genfld, gen0, gen1, genmask,
#ifdef USE_EXF_INTERPOLATION
     &     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     &     gen_nlon, gen_nlat, gen_xout, gen_yout, interp_method,
#endif
     &     myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE EXF_SET_GEN
C     ==================================================================
C
C     o set external forcing gen
C
C     started: Ralf.Giering@FastOpt.de 25-Mai-2000
C     changed: heimbach@mit.edu 10-Jan-2002
C              20-Dec-2002: mods for pkg/seaice, menemenlis@jpl.nasa.gov
C              heimbach@mit.edu: totally re-organized exf_set_...
C              replaced all routines by one generic routine
C              5-Aug-2003: added USE_EXF_INTERPOLATION for arbitrary
C                          input grid capability
C     11-Dec-2006 added time-mean and monthly-mean climatology options
C        genperiod=0 means input file is one time-constant field
C        genperiod=-12 means input file contains 12 monthly means

C     ==================================================================
C     SUBROUTINE EXF_SET_GEN
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==
      _RL genstartdate, genperiod
      _RL exf_inscal_gen
      _RL genremove_intercept, genremove_slope
      _RL genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      CHARACTER*1 genmask
      CHARACTER*(128) genfile
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef USE_EXF_INTERPOLATION
C     gen_lon_0 ,gen_lat_0 :: longitude and latitude of SouthWest
C                             corner of global input grid
C     gen_nlon, gen_nlat   :: input x-grid and y-grid size
C     gen_lon_inc          :: scalar x-grid increment
C     gen_lat_inc          :: vector y-grid increments
C     gen_xout, gen_yout   :: coordinates for output grid
      _RL gen_lon0, gen_lon_inc
      _RL gen_lat0, gen_lat_inc(MAX_LAT_INC)
      INTEGER gen_nlon, gen_nlat
      _RS gen_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS gen_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER interp_method
#endif /* USE_EXF_INTERPOLATION */

C     == Functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL first, changed
      INTEGER count0, count1
      INTEGER year0, year1
      INTEGER bi, bj, i, j
      _RL     fac
      CHARACTER*(128) genfile0, genfile1

C     == end of interface ==

      IF ( genfile .NE. ' ' .AND. genperiod .NE. 0. ) THEN

cph(
cph-exf-print         if (genfile .EQ. hfluxfile)  year0 = 3000
cph)

         IF ( genperiod .EQ. -12. ) THEN
C-    genperiod=-12 means input file contains 12 monthly means
C     records, corresponding to Jan. (rec=1) through Dec. (rec=12)
            CALL cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1,
     I           myTime, myIter, myThid )
         ELSEIF ( genperiod .LT. 0. ) THEN
           j = ILNBLNK(genfile)
           WRITE(msgBuf,'(A,1PE16.8,2A)')
     &        'EXF_SET_GEN: Invalid genperiod=', genperiod,
     &        ' for file: ', genfile(1:j)
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R EXF_SET_GEN'
         ELSE
C-    get record numbers and interpolation factor for gen
            CALL exf_GetFFieldRec(
     I           genstartdate, genperiod,
     I           useExfYearlyFields,
     O           fac, first, changed,
     O           count0, count1, year0, year1,
     I           myTime, myIter, myThid )

         ENDIF
         IF ( exf_debugLev.GE.debLevD ) THEN
           _BEGIN_MASTER( myThid )
           j = ILNBLNK(genfile)
           WRITE(msgBuf,'(3A)') ' EXF_SET_GEN: ',
     &       'processing file: ', genfile(1:j)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2A,I10,2I7)') ' EXF_SET_GEN: ',
     &       ' myIter, count0, count1:', myIter, count0, count1
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2A,2(L2,2X),E16.9)') ' EXF_SET_GEN: ',
     &       ' first, changed, fac:  ', first, changed, fac
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
         ENDIF

         IF ( first ) THEN
            CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, genperiod, year0,
     I         genfile,
     O         genfile0,
     I         myTime, myIter, myThid )
            IF ( exf_debugLev.GE.debLevC ) THEN
              _BEGIN_MASTER(myThid)
              j = ILNBLNK(genfile0)
              WRITE(msgBuf,'(A,I10,A,I6,2A)')
     &        ' EXF_SET_GEN: it=', myIter, ' loading rec=', count0,
     &        ' from: ', genfile0(1:j)
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
              _END_MASTER(myThid)
            ENDIF

#ifdef USE_EXF_INTERPOLATION
            IF ( interp_method.GE.1 ) THEN
              CALL EXF_INTERP(
     I             genfile0, exf_iprec,
     O             gen1,
     I             count0, gen_xout, gen_yout,
     I             gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     I             gen_nlon, gen_nlat, interp_method, myIter, myThid )
            ELSE
#endif /* USE_EXF_INTERPOLATION */
              CALL READ_REC_3D_RL( genfile0, exf_iprec, 1,
     &                             gen1, count0, myIter, myThid )
#ifdef USE_EXF_INTERPOLATION
            ENDIF
#endif /* USE_EXF_INTERPOLATION */

            CALL exf_filter_rl( gen1, genmask, myThid )
         ENDIF

         IF ( first .OR. changed ) THEN
            CALL exf_SwapFFields( gen0, gen1, myThid )

            CALL exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, genperiod, year1,
     I         genfile,
     O         genfile1,
     I         myTime, myIter, myThid )
            IF ( exf_debugLev.GE.debLevC ) THEN
              _BEGIN_MASTER(myThid)
              j = ILNBLNK(genfile1)
              WRITE(msgBuf,'(A,I10,A,I6,2A)')
     &        ' EXF_SET_GEN: it=', myIter, ' loading rec=', count1,
     &        ' from: ', genfile1(1:j)
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
              _END_MASTER(myThid)
            ENDIF

#ifdef USE_EXF_INTERPOLATION
            IF ( interp_method.GE.1 ) THEN
              CALL EXF_INTERP(
     I             genfile1, exf_iprec,
     O             gen1,
     I             count1, gen_xout, gen_yout,
     I             gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     I             gen_nlon, gen_nlat, interp_method, myIter, myThid )
            ELSE
#endif /* USE_EXF_INTERPOLATION */
              CALL READ_REC_3D_RL( genfile1, exf_iprec, 1,
     &                             gen1, count1, myIter, myThid )
#ifdef USE_EXF_INTERPOLATION
            ENDIF
#endif /* USE_EXF_INTERPOLATION */

            CALL exf_filter_rl( gen1, genmask, myThid )
         ENDIF

C     Loop over tiles.
         DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),mybxhi(myThid)
           DO j = 1,sny
            DO i = 1,snx
C     Interpolate linearly onto the  time.
             genfld(i,j,bi,bj) = exf_inscal_gen * (
     &                          fac * gen0(i,j,bi,bj) +
     &              (exf_one - fac) * gen1(i,j,bi,bj) )
             genfld(i,j,bi,bj) =
     &            genfld(i,j,bi,bj) -
     &            exf_inscal_gen * ( genremove_intercept +
     &            genremove_slope*(myTime-startTime) )
            ENDDO
           ENDDO
          ENDDO
         ENDDO

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_INIT_GEN (
     &     genfile, genperiod, exf_inscal_gen, genmask,
     &     genconst, genfld, gen0, gen1,
#ifdef USE_EXF_INTERPOLATION
     &     gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     &     gen_nlon, gen_nlat, gen_xout, gen_yout, interp_method,
#endif
     &     myThid )

C     ==================================================================
C     SUBROUTINE EXF_INIT_GEN
C     ==================================================================
C
C     started: Ralf.Giering@FastOpt.de 25-Mai-2000
C     changed: heimbach@mit.edu 10-Jan-2002
C              heimbach@mit.edu: totally re-organized exf_set_...
C              replaced all routines by one generic routine
C
C     ==================================================================
C     SUBROUTINE EXF_INIT_GEN
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "EXF_PARAM.h"

C     == routine arguments ==
      _RL genperiod, exf_inscal_gen, genconst
      _RL genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      CHARACTER*1 genmask
      CHARACTER*(128) genfile
      INTEGER myThid

#ifdef USE_EXF_INTERPOLATION
C     gen_lon_0 ,gen_lat_0 :: longitude and latitude of SouthWest
C                             corner of global input grid
C     gen_nlon, gen_nlat   :: input x-grid and y-grid size
C     gen_lon_inc          :: scalar x-grid increment
C     gen_lat_inc          :: vector y-grid increments
C     gen_xout, gen_yout   :: coordinates for output grid
      _RL gen_lon0, gen_lon_inc
      _RL gen_lat0, gen_lat_inc(MAX_LAT_INC)
      INTEGER gen_nlon, gen_nlat
      _RS gen_xout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS gen_yout  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER interp_method
#endif /* USE_EXF_INTERPOLATION */

C     == Functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, i, j, count

C     == end of interface ==

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), mybxhi(myThid)
          DO j = 1-oly, sny+oly
            DO i = 1-olx, snx+olx
              genfld(i,j,bi,bj)  = genconst
              gen0(i,j,bi,bj)    = genconst
              gen1(i,j,bi,bj)    = genconst
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF ( genfile .NE. ' ' .AND. genperiod .EQ. 0. ) THEN
         count = 1
         IF ( exf_debugLev.GE.debLevC ) THEN
           _BEGIN_MASTER(myThid)
           j = ILNBLNK(genfile)
           WRITE(msgBuf,'(A,I10,A,I6,2A)')
     &        ' EXF_INIT_GEN: it=', -1, ' loading rec=', count,
     &        ' from: ', genfile(1:j)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           _END_MASTER(myThid)
         ENDIF

#ifdef USE_EXF_INTERPOLATION
         IF ( interp_method.GE.1 ) THEN
              CALL EXF_INTERP(
     I             genfile, exf_iprec,
     O             genfld,
     I             count, gen_xout, gen_yout,
     I             gen_lon0, gen_lon_inc, gen_lat0, gen_lat_inc,
     I             gen_nlon, gen_nlat, interp_method, 0, myThid )
         ELSE
#endif /* USE_EXF_INTERPOLATION */
            CALL READ_REC_3D_RL( genfile, exf_iprec, 1,
     &                             genfld, count, 0, myThid )
#ifdef USE_EXF_INTERPOLATION
         ENDIF
#endif /* USE_EXF_INTERPOLATION */

         CALL exf_filter_rl( genfld, genmask, myThid )

C     Loop over tiles and scale genfld
         DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),mybxhi(myThid)
               DO j = 1,sny
                  DO i = 1,snx
                     genfld(i,j,bi,bj) =
     &                    exf_inscal_gen * genfld(i,j,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO

      ENDIF

      RETURN
      END
