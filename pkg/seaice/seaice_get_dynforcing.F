C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_get_dynforcing.F,v 1.14 2012/11/09 22:15:18 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE SEAICE_GET_DYNFORCING(
     I     uIce, vIce,
     O     taux, tauy,
     I     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_GET_DYNFORCING
C     |   compute surface stress from atmopheric forcing fields
C     *==========================================================*
C     | started by Martin Losch, April 2007
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
# include "EXF_PARAM.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C   uIce   (inp) :: zonal      ice velocity (input)
C   vIce   (inp) :: meridional ice velocity (input)
C   taux   (out) :: zonal      wind stress over ice at U point
C   tauy   (out) :: meridional wind stress over ice at V point
C   myTime (inp) :: current time in simulation
C   myIter (inp) :: iteration number in simulation
C   myThid (inp) :: my Thread Id. number
      _RL uIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL taux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_CGRID
C     !LOCAL VARIABLES:
C     i,j,bi,bj :: Loop counters
C     ks        :: vertical index of surface layer
      INTEGER bi, bj, i, j
      INTEGER ks
      _RL  COSWIN
      _RS  SINWIN
      _RL  U1, V1, AAA
C     CDAIR   :: local wind stress coefficient (used twice)
C     oceTauX :: wind-stress over open-ocean (on Arakawa A-grid), X direction
C     oceTauY :: wind-stress over open-ocean (on Arakawa A-grid), Y direction
      _RL CDAIR   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef SEAICE_EXTERNAL_FLUXES
      _RL oceTauX (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oceTauY (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

C--   surface level
      ks = 1
C--   introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   NOW SET UP FORCING FIELDS

#ifdef SEAICE_EXTERNAL_FLUXES
      IF (useAtmWind) THEN
#endif
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
     &                            *_maskW(i,j,ks,bi,bj)
          fv(i,j,bi,bj) = 0.5 _d 0*( oceTauY(i,j) + oceTauY(i,j-1) )
     &                            *_maskS(i,j,ks,bi,bj)
         ENDDO
        ENDDO
#endif /* ndef SEAICE_EXTERNAL_FLUXES */

C--   Now compute ice surface stress
        IF (useRelativeWind) THEN
         DO j=1-Oly,sNy+Oly-1
          DO i=1-Olx,sNx+Olx-1
           U1=UWIND(i,j,bi,bj)
     &          + 0.5 _d 0 * (uVel(i,j,ks,bi,bj)+uVel(i+1,j,ks,bi,bj))
     &          - 0.5 _d 0 * (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj))
           V1=VWIND(i,j,bi,bj)
     &          + 0.5 _d 0 * (vVel(i,j,ks,bi,bj)+vVel(i,j+1,ks,bi,bj))
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
     &         ( CDAIR(i,j)*(COSWIN*
     &          (uWind(i,j,bi,bj)
     &          +0.5 _d 0*(uVel(i,j,ks,bi,bj)+uVel(i+1,j,ks,bi,bj))
     &          -0.5 _d 0*(uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)))
     &          -SIGN(SINWIN, _fCori(i,j,bi,bj))*
     &          (vWind(i,j,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,ks,bi,bj)+vVel(i,j+1,ks,bi,bj))
     &          -0.5 _d 0*(vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))))
     &          +CDAIR(i-1,j)*(COSWIN*
     &          (uWind(i-1,j,bi,bj)
     &          +0.5 _d 0*(uVel(i-1,j,ks,bi,bj)+uVel(i,j,ks,bi,bj))
     &          -0.5 _d 0*(uIce(i-1,j,bi,bj)+uIce(i,j,bi,bj)))
     &          -SIGN(SINWIN, _fCori(i-1,j,bi,bj))*
     &          (vWind(i-1,j,bi,bj)
     &          +0.5 _d 0*(vVel(i-1,j,ks,bi,bj)+vVel(i-1,j+1,ks,bi,bj))
     &          -0.5 _d 0*(vIce(i-1,j,bi,bj)+vIce(i-1,j+1,bi,bj))))
     &         )*_maskW(i,j,ks,bi,bj)
C     interpolate to V points
           tauy(i,j,bi,bj)= 0.5 _d 0 *
     &         ( CDAIR(i,j)*(SIGN(SINWIN, _fCori(i,j,bi,bj))*
     &          (uWind(i,j,bi,bj)
     &          +0.5 _d 0*(uVel(i,j,ks,bi,bj)+uVel(i+1,j,ks,bi,bj))
     &          -0.5 _d 0*(uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj)))
     &          +COSWIN*
     &          (vWind(i,j,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,ks,bi,bj)+vVel(i,j+1,ks,bi,bj))
     &          -0.5 _d 0*(vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))))
     &          +CDAIR(i,j-1)*(SIGN(SINWIN, _fCori(i,j-1,bi,bj))*
     &          (uWind(i,j-1,bi,bj)
     &          +0.5 _d 0*(uVel(i,j-1,ks,bi,bj)+uVel(i+1,j-1,ks,bi,bj))
     &          -0.5 _d 0*(uIce(i,j-1,bi,bj)+uIce(i+1,j-1,bi,bj)))
     &          +COSWIN*
     &          (vWind(i,j-1,bi,bj)
     &          +0.5 _d 0*(vVel(i,j-1,ks,bi,bj)+vVel(i,j,ks,bi,bj))
     &          -0.5 _d 0*(vIce(i,j-1,bi,bj)+vIce(i,j,bi,bj))))
     &         )*_maskS(i,j,ks,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
C     interpolate to U points
           taux(i,j,bi,bj)=0.5 _d 0 *
     &         (  CDAIR(i  ,j)*(
     &          COSWIN                            *uWind(i  ,j,bi,bj)
     &          -SIGN(SINWIN, _fCori(i  ,j,bi,bj))*vWind(i  ,j,bi,bj) )
     &          + CDAIR(i-1,j)*(
     &          COSWIN                            *uWind(i-1,j,bi,bj)
     &          -SIGN(SINWIN, _fCori(i-1,j,bi,bj))*vWind(i-1,j,bi,bj) )
     &         )*_maskW(i,j,ks,bi,bj)
C     interpolate to V points
           tauy(i,j,bi,bj)=0.5 _d 0 *
     &         (  CDAIR(i,j  )*(
     &          SIGN(SINWIN, _fCori(i,j  ,bi,bj))*uWind(i,j  ,bi,bj)
     &          +COSWIN*vWind(i,j  ,bi,bj) )
     &          + CDAIR(i,j-1)*(
     &          SIGN(SINWIN, _fCori(i,j-1,bi,bj))*uWind(i,j-1,bi,bj)
     &          +COSWIN*vWind(i,j-1,bi,bj) )
     &         )*_maskS(i,j,ks,bi,bj)
          ENDDO
         ENDDO
        ENDIF

       ENDDO
      ENDDO

#ifdef SEAICE_EXTERNAL_FLUXES
      ELSE
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
          taux(i,j,bi,bj) = CDAIR(i,j)*fu(i,j,bi,bj)
     &                    *_maskW(i,j,ks,bi,bj)
          tauy(i,j,bi,bj) = CDAIR(i,j)*fv(i,j,bi,bj)
     &                    *_maskS(i,j,ks,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF
#endif /* SEAICE_EXTERNAL_FLUXES */
#endif /* SEAICE_CGRID */

      RETURN
      END
