C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_get_dynforcing.F,v 1.1 2007/04/17 16:13:53 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_GET_DYNFORCING( 
     I     uIce, vIce,
     O     taux, tauy, WINDX, WINDY,
     I     myTime, myIter, myThid )
C     /================================================================\
C     | SUBROUTINE seaice_get_dynforcing                               |
C     |   compute surface stress from atmopheric forcing fields        |
C     |================================================================|
C     | started by Martin Losch, April 2007                            |
C     \================================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"

C     === Routine arguments ===
C     INPUT:
C     uIce   - zonal      ice velocity (currently not used)
C     vIce   - meridional ice velocity (currently not used)
C     taux   - zonal      wind stress over ice at U point
C     tauy   - meridional wind stress over ice at V point
C     WINDX  - zonal      wind stress over water at C points
C     WINDY  - meridional wind stress over water at C points
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL uIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL vIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL taux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL tauy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL WINDX   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL WINDY   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
C     === Local variables ===
C     i,j - Loop counters
      INTEGER bi, bj, i, j
      _RL  U1, V1, AAA
      _RL  RHOAIR, SINWIN, COSWIN
C     CDAIR  - local wind stress coefficient (used twice)
      _RL CDAIR(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C--   FIRST SET UP BASIC CONSTANTS
      RHOAIR = SEAICE_rhoAir

C--   introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   NOW SET UP FORCING FIELDS

#ifdef ALLOW_ATM_WIND
C--   Wind stress is computed on center of C-grid cell and interpolated
C     to U and V points later
C     locations from wind on tracer locations
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          U1=UWIND(I,J,bi,bj)
          V1=VWIND(I,J,bi,bj)
          AAA=U1**2+V1**2
          IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
             AAA=SEAICE_EPS
          ELSE
             AAA=SQRT(AAA)
          ENDIF
C first ocean surface stress at C-points
          CDAIR(I,J)=SEAICE_rhoAir*OCEAN_drag
     &         *(2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
          WINDX(I,J,bi,bj)=CDAIR(I,J)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(I,J,bi,bj))*V1)
          WINDY(I,J,bi,bj)=CDAIR(I,J)*
     &         (SIGN(SINWIN, _fCori(I,J,bi,bj))*U1+COSWIN*V1)
C now ice surface stress
          CDAIR(I,J) = SEAICE_rhoAir*SEAICE_drag*AAA
         ENDDO
        ENDDO
C     now interpolate to U and V points respectively
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          taux(I,J,bi,bj)=0.5 _d 0 * 
     &         ( CDAIR(I  ,J)*(
     &         COSWIN                            *uWind(I  ,J,bi,bj)
     &         -SIGN(SINWIN, _fCori(I  ,J,bi,bj))*vWind(I  ,J,bi,bj) )
     &         + CDAIR(I-1,J)*(
     &         COSWIN                            *uWind(I-1,J,bi,bj)
     &         -SIGN(SINWIN, _fCori(I-1,J,bi,bj))*vWind(I-1,J,bi,bj) )
     &         )
C     interpolate to V point
          tauy(I,J,bi,bj)=0.5 _d 0 * 
     &         ( CDAIR(I,J  )*(
     &         SIGN(SINWIN, _fCori(I,J  ,bi,bj))*uWind(I,J  ,bi,bj)
     &         +COSWIN*vWind(I,J  ,bi,bj) )
     &         + CDAIR(I,J-1)*(
     &         SIGN(SINWIN, _fCori(I,J-1,bi,bj))*uWind(I,J-1,bi,bj)
     &         +COSWIN*vWind(I,J-1,bi,bj) )
     &         )
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#else
C--   Wind stress is available on U and V points, copy it to seaice variables.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
C first ocean surface stress at C-points (is that needed at all?)
          WINDX(I,J,bi,bj)=.5 _d 0 * (FU(I,J,bi,bj)+FU(I+1,J,bi,bj))
          WINDY(I,J,bi,bj)=.5 _d 0 * (FV(I,J,bi,bj)+FV(I,J+1,bi,bj))
         ENDDO
        ENDDO
C     now interpolate to U and V points respectively
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C now ice surface stress
          CDAIR(I,J)       = SEAICE_drag/OCEAN_drag
          taux (I,J,bi,bj) = CDAIR(I,J)*FU(I,J,bi,bj)
          tauy (I,J,bi,bj) = CDAIR(I,J)*FV(I,J,bi,bj) 
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_ATM_WIND */
#endif /* SEAICE_CGRID */

      RETURN
      END
