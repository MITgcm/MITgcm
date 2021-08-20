#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_CHECK_RANGE( myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE EXF_CHECK_RANGE
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
c#include "EXF_CONSTANTS.h"
#include "EXF_FIELDS.h"

C     == routine arguments ==
C     myThid - thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter, myThid

C     == local variables ==
      INTEGER i, j, ks, bi, bj
      INTEGER exferr

C     == end of interface ==

      exferr = 0

C--   Only master thread can safely write directly to standard output:
      _BARRIER
      _BEGIN_MASTER( myThid )

      ks = 1
      IF ( usingPCoords ) ks = Nr
c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO bj = 1, nSy
       DO bi = 1, nSx

C Change checking range because some atmospheric fields will
C not always have valid values in the tile edges.
c       DO j = 1-OLy, sNy+OLy
c        DO i = 1-OLx, sNx+OLx
        DO j = 1, sNy
         DO i = 1, sNx

C     Heat flux.
          IF ( ( hflux(i,j,bi,bj) .GT. 1600. .OR.
     &         hflux(i,j,bi,bj) .LT. -500. ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: hflux out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, hflux(i,j,bi,bj)
           exferr = 1
          ENDIF

C     Freshwater flux.
          IF ( ABS(sflux(i,j,bi,bj)) .GT. 1.E-6 .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: sflux out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, sflux(i,j,bi,bj)
           exferr = 1
          ENDIF

C     Zonal wind stress.
          IF ( ABS(ustress(i,j,bi,bj)) .GT. 2.7 .AND.
     &         maskW(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: ustress out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, ustress(i,j,bi,bj)
           exferr = 1
          ENDIF

C     Meridional wind stress.
          IF ( ABS(vstress(i,j,bi,bj)) .GT. 2.3 .AND.
     &         maskS(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: vstress out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, vstress(i,j,bi,bj)
           exferr = 1
          ENDIF

          IF ( useAtmWind ) THEN
C     zonal wind speed
           IF ( ABS(uwind(i,j,bi,bj)) .GT. 100. .AND.
     &         maskW(i,j,ks,bi,bj) .NE. 0. ) THEN
            WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: uwind out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, uwind(i,j,bi,bj)
            exferr = 1
           ENDIF

C     zonal wind speed
           IF ( ABS(vwind(i,j,bi,bj)) .GT. 100. .AND.
     &         maskS(i,j,ks,bi,bj) .NE. 0. ) THEN
            WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: vwind out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, vwind(i,j,bi,bj)
            exferr = 1
           ENDIF
          ENDIF

C     wind speed modulus
          IF ( ( wspeed(i,j,bi,bj) .LT. 0. .OR.
     &         wspeed(i,j,bi,bj) .GT. 100. ) .AND.
     &         maskS(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: wspeed out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, wspeed(i,j,bi,bj)
           exferr = 1
          ENDIF

#ifdef ALLOW_ATM_TEMP
C     2-m air temperature
          IF ( (atemp(i,j,bi,bj) .LT. 183 .OR.
     &         atemp(i,j,bi,bj) .GT. 343 ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(2A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: atemp + exf_offset_atemp ',
     &          'out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, atemp(i,j,bi,bj)
           exferr = 1
          ENDIF

C     2-m specific humidity
          IF ( (aqh(i,j,bi,bj) .LT. 0. .OR.
     &         aqh(i,j,bi,bj) .GT. 0.1 ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: aqh out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, aqh(i,j,bi,bj)
           exferr = 1
          ENDIF

C     precipitation rate
          IF ( (precip(i,j,bi,bj) .LT. 0. .OR.
     &         precip(i,j,bi,bj) .GT. 2.E-6 ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: precip out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, precip(i,j,bi,bj)
           exferr = 1
          ENDIF

C     snow
          IF ( (snowprecip(i,j,bi,bj) .LT. 0. .OR.
     &         snowprecip(i,j,bi,bj) .GT. 2.E-6 ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(2A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: snowprecip out of range ',
     &          'for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, snowprecip(i,j,bi,bj)
           exferr = 1
          ENDIF
#endif

#ifdef SHORTWAVE_HEATING
C     Short wave radiative flux.
          IF ( (swflux(i,j,bi,bj) .GT. 1. .OR.
     &         swflux(i,j,bi,bj) .LT. -1000. ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: swflux out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, swflux(i,j,bi,bj)
           exferr = 1
          ENDIF
#endif

#ifdef ALLOW_RUNOFF
C     Runoff.
          IF ( (runoff(i,j,bi,bj) .LT. 0. .OR.
     &         runoff(i,j,bi,bj) .GT. 1.E-6 ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: runoff out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, runoff(i,j,bi,bj)
           WRITE(standardMessageUnit,'(A)')
     &          'Please note that input units for runoff are'
           WRITE(standardMessageUnit,'(A)')
     &          'm/s not m/yr.  If input file is in m/yr, set'
           WRITE(standardMessageUnit,'(A)')
     &          'exf_inscal_runoff=3.170979198E-8'
           WRITE(standardMessageUnit,'(A)')
     &          'in the data.exf input file.'
           exferr = 1
          ENDIF
# ifdef ALLOW_RUNOFTEMP
C     Runoff temperature.
          IF ( (runoftemp(i,j,bi,bj) .LT. -2. .OR.
     &         runoff(i,j,bi,bj) .GT. 36 ) .AND.
     &         maskC(i,j,ks,bi,bj) .NE. 0. ) THEN
           WRITE(standardMessageUnit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: runoftemp out of range at bi,bj,i,j,it= ',
     &          bi, bj, i, j, myIter, runoff(i,j,bi,bj)
           exferr = 1
          ENDIF
# endif /* ALLOW_RUNOFTEMP */
#endif /* ALLOW_RUNOFF */
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      IF ( exferr .NE. 0 ) THEN
       WRITE(standardMessageUnit,'(A)')
     &      'EXF WARNING: If you think these values are OK '
       WRITE(standardMessageUnit,'(A)')
     &      'EXF WARNING: then set useExfCheckRange=.FALSE.'
       STOP 'ABNORMAL END: S/R EXF_CHECK_RANGE'
      ENDIF
      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
