C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_get_dynforcing.F,v 1.5 2008/01/15 23:24:38 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_GET_DYNFORCING( 
     I     uIce, vIce,
     O     taux, tauy,
     I     myTime, myIter, myThid )
C     *================================================================*
C     | SUBROUTINE SEAICE_GET_DYNFORCING
C     |   compute surface stress from atmopheric forcing fields
C     *================================================================*
C     | started by Martin Losch, April 2007
C     *================================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
# include "EXF_PARAM.h"
#endif

C     === Routine arguments ===
C     INPUT:
C     uIce   - zonal      ice velocity (currently not used)
C     vIce   - meridional ice velocity (currently not used)
C     taux   - zonal      wind stress over ice at U point
C     tauy   - meridional wind stress over ice at V point
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL uIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL vIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL taux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL tauy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
C     === Local variables ===
C     i,j - Loop counters
      INTEGER bi, bj, i, j
      _RL  U1, V1, AAA
      _RL  SINWIN, COSWIN
C     CDAIR   :: local wind stress coefficient (used twice)
C     oceTauX :: wind-stress over open-ocean (on Arakawa A-grid), X direction
C     oceTauY :: wind-stress over open-ocean (on Arakawa A-grid), Y direction
      _RL CDAIR   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef SEAICE_EXTERNAL_FLUXES
      _RL oceTauX (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oceTauY (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

C--   introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   NOW SET UP FORCING FIELDS

#if !defined(SEAICE_EXTERNAL_FLUXES) || defined(ALLOW_ATM_WIND)
C--   Wind stress is computed on center of C-grid cell
C     and interpolated to U and V points later
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifndef SEAICE_EXTERNAL_FLUXES
C--   First compute wind-stress over open ocean: this will results in
C     over-writing fu and fv that were computed or read-in by pkg/exf.
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
          CDAIR(I,J)=SEAICE_rhoAir*OCEAN_drag
     &         *(2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
          oceTauX(I,J)=CDAIR(I,J)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(I,J,bi,bj))*V1)
          oceTauY(I,J)=CDAIR(I,J)*
     &         (SIGN(SINWIN, _fCori(I,J,bi,bj))*U1+COSWIN*V1)
         ENDDO
        ENDDO
C--   Interpolate wind stress over open ocean (N/m^2)
C     from A-grid to U and V points of C-grid 
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          fu(I,J,bi,bj) = 0.5 _d 0*( oceTauX(I,J) + oceTauX(I-1,J) )
          fv(I,J,bi,bj) = 0.5 _d 0*( oceTauY(I,J) + oceTauY(I,J-1) )
         ENDDO
        ENDDO
#endif /* ndef SEAICE_EXTERNAL_FLUXES */

C--   Now compute ice surface stress
        IF (useRelativeWind) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           U1=UWIND(I,J,bi,bj)
     &          + 0.5 _d 0 * (uVel(i,j,1,bi,bj)+uVel(i+1,j,1,bi,bj))
     &          - 0.5 _d 0 * (uice(i,j,1,bi,bj)+uice(i+1,j,1,bi,bj))
           V1=VWIND(I,J,bi,bj)
     &          + 0.5 _d 0 * (vVel(i,j,1,bi,bj)+vVel(i,j+1,1,bi,bj))
     &          - 0.5 _d 0 * (vice(i,j,1,bi,bj)+vice(i,j+1,1,bi,bj))
           AAA=U1**2+V1**2
           IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
              AAA=SEAICE_EPS
           ELSE
              AAA=SQRT(AAA)
           ENDIF
           CDAIR(I,J) = SEAICE_rhoAir*SEAICE_drag*AAA
          ENDDO
         ENDDO
        ELSE
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
           CDAIR(I,J) = SEAICE_rhoAir*SEAICE_drag*AAA
          ENDDO
         ENDDO
        ENDIF
        IF (useRelativeWind) THEN
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
C     interpolate to U points
           taux(I,J,bi,bj)= 0.5 _d 0 *
     &          (CDAIR(I,J)*(COSWIN*
     &          (uWind(I,J,bi,bj)
     &          +0.5 _d 0*(uVel(i,j,1,bi,bj)+uVel(i+1,j,1,bi,bj))
     &          -0.5 _d 0*(uice(i,j,1,bi,bj)+uice(i+1,j,1,bi,bj)))
     &          -SIGN(SINWIN, _fCori(I,J,bi,bj))*
     &          (vWind(I,J,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,1,bi,bj)+vVel(i,j+1,1,bi,bj))
     &          -0.5 _d 0*(vice(i,j,1,bi,bj)+vice(i,j+1,1,bi,bj))))
     &          +CDAIR(I-1,J)*(COSWIN*
     &          (uWind(I-1,J,bi,bj)
     &          +0.5 _d 0*(uVel(i-1,j,1,bi,bj)+uVel(i,j,1,bi,bj))
     &          -0.5 _d 0*(uice(i-1,j,1,bi,bj)+uice(i,j,1,bi,bj)))
     &          -SIGN(SINWIN, _fCori(I-1,J,bi,bj))*
     &          (vWind(I-1,J,bi,bj)
     &          +0.5 _d 0*(vVel(i-1,j,1,bi,bj)+vVel(i-1,j+1,1,bi,bj))
     &          -0.5 _d 0*(vice(i-1,j,1,bi,bj)+vice(i-1,j+1,1,bi,bj)))))
C     interpolate to V points
           tauy(I,J,bi,bj)= 0.5 _d 0 * 
     &          (CDAIR(I,J)*(SIGN(SINWIN, _fCori(I,J,bi,bj))*
     &          (uWind(I,J,bi,bj)
     &          +0.5 _d 0*(uVel(i,j,1,bi,bj)+uVel(i+1,j,1,bi,bj))
     &          -0.5 _d 0*(uice(i,j,1,bi,bj)+uice(i+1,j,1,bi,bj)))
     &          +COSWIN*
     &          (vWind(I,J,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,1,bi,bj)+vVel(i,j+1,1,bi,bj))
     &          -0.5 _d 0*(vice(i,j,1,bi,bj)+vice(i,j+1,1,bi,bj))))
     &          +CDAIR(I,J-1)*(SIGN(SINWIN, _fCori(I,J-1,bi,bj))*
     &          (uWind(I,J-1,bi,bj)
     &          +0.5 _d 0*(uVel(i,j-1,1,bi,bj)+uVel(i+1,j-1,1,bi,bj))
     &          -0.5 _d 0*(uice(i,j-1,1,bi,bj)+uice(i+1,j-1,1,bi,bj)))
     &          +COSWIN*
     &          (vWind(I,J-1,bi,bj)
     &          +0.5 _d 0*(vVel(i,j-1,1,bi,bj)+vVel(i,j,1,bi,bj))
     &          -0.5 _d 0*(vice(i,j-1,1,bi,bj)+vice(i,j,1,bi,bj)))))
          ENDDO
         ENDDO
        ELSE
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
C     interpolate to U points
           taux(I,J,bi,bj)=0.5 _d 0 * 
     &          ( CDAIR(I  ,J)*(
     &          COSWIN                            *uWind(I  ,J,bi,bj)
     &          -SIGN(SINWIN, _fCori(I  ,J,bi,bj))*vWind(I  ,J,bi,bj) )
     &          + CDAIR(I-1,J)*(
     &          COSWIN                            *uWind(I-1,J,bi,bj)
     &          -SIGN(SINWIN, _fCori(I-1,J,bi,bj))*vWind(I-1,J,bi,bj) )
     &          )
C     interpolate to V points
           tauy(I,J,bi,bj)=0.5 _d 0 * 
     &          ( CDAIR(I,J  )*(
     &          SIGN(SINWIN, _fCori(I,J  ,bi,bj))*uWind(I,J  ,bi,bj)
     &          +COSWIN*vWind(I,J  ,bi,bj) )
     &          + CDAIR(I,J-1)*(
     &          SIGN(SINWIN, _fCori(I,J-1,bi,bj))*uWind(I,J-1,bi,bj)
     &          +COSWIN*vWind(I,J-1,bi,bj) )
     &          )
          ENDDO
         ENDDO
        ENDIF

       ENDDO
      ENDDO
#else /* not SEAICE_EXTERNAL_FLUXES or ALLOW_ATM_WIND */ 
C--   Wind stress is available on U and V points, copy it to seaice variables.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
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
#endif /* not SEAICE_EXTERNAL_FLUXES or ALLOW_ATM_WIND */ 
#endif /* SEAICE_CGRID */

      RETURN
      END
