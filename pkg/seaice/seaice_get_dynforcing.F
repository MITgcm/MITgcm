C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_get_dynforcing.F,v 1.11 2011/10/11 14:07:35 jmc Exp $
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
      _RL uIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL taux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
C     === Local variables ===
C     i,j - Loop counters
C     k   - vertical index of surface layer
      INTEGER bi, bj, i, j
      INTEGER k
      _RL  COSWIN
      _RS  SINWIN
#if !defined(SEAICE_EXTERNAL_FLUXES) || defined(ALLOW_ATM_WIND)
      _RL  U1, V1, AAA
#endif
C     CDAIR   :: local wind stress coefficient (used twice)
C     oceTauX :: wind-stress over open-ocean (on Arakawa A-grid), X direction
C     oceTauY :: wind-stress over open-ocean (on Arakawa A-grid), Y direction
      _RL CDAIR   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef SEAICE_EXTERNAL_FLUXES
      _RL oceTauX (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oceTauY (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

C--   surrface level
      k = 1
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
          U1=UWIND(i,j,bi,bj)
          V1=VWIND(i,j,bi,bj)
          AAA=U1**2+V1**2
          IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
             AAA=SEAICE_EPS
          ELSE
             AAA=SQRT(AAA)
          ENDIF
          CDAIR(i,j)=SEAICE_rhoAir*OCEAN_drag
     &         *(2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
          oceTauX(i,j)=CDAIR(i,j)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(i,j,bi,bj))*V1)
          oceTauY(i,j)=CDAIR(i,j)*
     &         (SIGN(SINWIN, _fCori(i,j,bi,bj))*U1+COSWIN*V1)
         ENDDO
        ENDDO
C--   Interpolate wind stress over open ocean (N/m^2)
C     from A-grid to U and V points of C-grid
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          fu(i,j,bi,bj) = 0.5 _d 0*( oceTauX(i,j) + oceTauX(i-1,j) )
          fv(i,j,bi,bj) = 0.5 _d 0*( oceTauY(i,j) + oceTauY(i,j-1) )
         ENDDO
        ENDDO
#endif /* ndef SEAICE_EXTERNAL_FLUXES */

C--   Now compute ice surface stress
        IF (useRelativeWind) THEN
         DO j=1-Oly,sNy+Oly-1
          DO i=1-Olx,sNx+Olx-1
           U1=UWIND(i,j,bi,bj)
     &          + 0.5 _d 0 * (uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))
     &          - 0.5 _d 0 * (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj))
           V1=VWIND(i,j,bi,bj)
     &          + 0.5 _d 0 * (vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))
     &          - 0.5 _d 0 * (vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))
           AAA=U1**2+V1**2
           IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
              AAA=SEAICE_EPS
           ELSE
              AAA=SQRT(AAA)
           ENDIF
           IF ( yC(i,j,bi,bj) .LT. ZERO ) THEN
            CDAIR(i,j) = SEAICE_rhoAir*SEAICE_drag_south*AAA
           ELSE
            CDAIR(i,j) = SEAICE_rhoAir*SEAICE_drag*AAA
           ENDIF
          ENDDO
         ENDDO
        ELSE
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           U1=UWIND(i,j,bi,bj)
           V1=VWIND(i,j,bi,bj)
           AAA=U1**2+V1**2
           IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
              AAA=SEAICE_EPS
           ELSE
              AAA=SQRT(AAA)
           ENDIF
           IF ( yC(i,j,bi,bj) .LT. ZERO ) THEN
            CDAIR(i,j) = SEAICE_rhoAir*SEAICE_drag_south*AAA
           ELSE
            CDAIR(i,j) = SEAICE_rhoAir*SEAICE_drag*AAA
           ENDIF
          ENDDO
         ENDDO
        ENDIF
        IF (useRelativeWind) THEN
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
C     interpolate to U points
           taux(i,j,bi,bj)= 0.5 _d 0 *
     &          (CDAIR(i,j)*(COSWIN*
     &          (uWind(i,j,bi,bj)
     &          +0.5 _d 0*(uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))
     &          -0.5 _d 0*(uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)))
     &          -SIGN(SINWIN, _fCori(i,j,bi,bj))*
     &          (vWind(i,j,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))
     &          -0.5 _d 0*(vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))))
     &          +CDAIR(i-1,j)*(COSWIN*
     &          (uWind(i-1,j,bi,bj)
     &          +0.5 _d 0*(uVel(i-1,j,k,bi,bj)+uVel(i,j,k,bi,bj))
     &          -0.5 _d 0*(uIce(i-1,j,bi,bj)+uIce(i,j,bi,bj)))
     &          -SIGN(SINWIN, _fCori(i-1,j,bi,bj))*
     &          (vWind(i-1,j,bi,bj)
     &          +0.5 _d 0*(vVel(i-1,j,k,bi,bj)+vVel(i-1,j+1,k,bi,bj))
     &          -0.5 _d 0*(vIce(i-1,j,bi,bj)+vIce(i-1,j+1,bi,bj)))))
C     interpolate to V points
           tauy(i,j,bi,bj)= 0.5 _d 0 *
     &          (CDAIR(i,j)*(SIGN(SINWIN, _fCori(i,j,bi,bj))*
     &          (uWind(i,j,bi,bj)
     &          +0.5 _d 0*(uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))
     &          -0.5 _d 0*(uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)))
     &          +COSWIN*
     &          (vWind(i,j,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj))
     &          -0.5 _d 0*(vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))))
     &          +CDAIR(i,j-1)*(SIGN(SINWIN, _fCori(i,j-1,bi,bj))*
     &          (uWind(i,j-1,bi,bj)
     &          +0.5 _d 0*(uVel(i,j-1,k,bi,bj)+uVel(i+1,j-1,k,bi,bj))
     &          -0.5 _d 0*(uIce(i,j-1,bi,bj)+uIce(i+1,j-1,bi,bj)))
     &          +COSWIN*
     &          (vWind(i,j-1,bi,bj)
     &          +0.5 _d 0*(vVel(i,j-1,k,bi,bj)+vVel(i,j,k,bi,bj))
     &          -0.5 _d 0*(vIce(i,j-1,bi,bj)+vIce(i,j,bi,bj)))))
          ENDDO
         ENDDO
        ELSE
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
C     interpolate to U points
           taux(i,j,bi,bj)=0.5 _d 0 *
     &          ( CDAIR(i  ,j)*(
     &          COSWIN                            *uWind(i  ,j,bi,bj)
     &          -SIGN(SINWIN, _fCori(i  ,j,bi,bj))*vWind(i  ,j,bi,bj) )
     &          + CDAIR(i-1,j)*(
     &          COSWIN                            *uWind(i-1,j,bi,bj)
     &          -SIGN(SINWIN, _fCori(i-1,j,bi,bj))*vWind(i-1,j,bi,bj) )
     &          )
C     interpolate to V points
           tauy(i,j,bi,bj)=0.5 _d 0 *
     &          ( CDAIR(i,j  )*(
     &          SIGN(SINWIN, _fCori(i,j  ,bi,bj))*uWind(i,j  ,bi,bj)
     &          +COSWIN*vWind(i,j  ,bi,bj) )
     &          + CDAIR(i,j-1)*(
     &          SIGN(SINWIN, _fCori(i,j-1,bi,bj))*uWind(i,j-1,bi,bj)
     &          +COSWIN*vWind(i,j-1,bi,bj) )
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
          IF ( yC(i,j,bi,bj) .LT. ZERO ) THEN
           CDAIR(i,j) = SEAICE_drag_south/OCEAN_drag
          ELSE
           CDAIR(i,j) = SEAICE_drag      /OCEAN_drag
          ENDIF
          taux (i,j,bi,bj) = CDAIR(i,j)*fu(i,j,bi,bj)
          tauy (i,j,bi,bj) = CDAIR(i,j)*fv(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* not SEAICE_EXTERNAL_FLUXES or ALLOW_ATM_WIND */
#endif /* SEAICE_CGRID */

      RETURN
      END
