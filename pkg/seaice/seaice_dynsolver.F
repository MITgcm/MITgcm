C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_dynsolver.F,v 1.10 2006/05/06 19:13:51 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_DYNSOLVER( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_DYNSOLVER                              |
C     | o Ice dynamics using LSR solver                          |
C     |   Zhang and Hibler,   JGR, 102, 8691-8702, 1997          |
C     |   or EVP explicit solver by Hunke and Dukowicz, JPO 27,  |
C     |   1849-1867 (1997)                                       |
C     |==========================================================|
C     | written by Martin Losch, March 2006                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL RHOICE, RHOAIR, SINWIN, COSWIN
      _RL PSTAR, AAA
      _RL U1, V1

C--   FIRST SET UP BASIC CONSTANTS
      RHOICE = SEAICE_rhoIce
      RHOAIR = SEAICE_rhoAir
      PSTAR  = SEAICE_strength

C--   introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   Compute proxy for geostrophic velocity,
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Oly,sNx+Olx
CML          GWATX(I,J,bi,bj)=uVel(i,j,KGEO(I,J,bi,bj),bi,bj)
CML          GWATY(I,J,bi,bj)=vVel(i,j,KGEO(I,J,bi,bj),bi,bj)
          GWATX(I,J,bi,bj)=uVel(i,j,1,bi,bj)
          GWATY(I,J,bi,bj)=vVel(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   NOW SET UP MASS PER UNIT AREA AND CORIOLIS TERM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          seaiceMassC(I,J,bi,bj)=RHOICE*HEFF(i,j,1,bi,bj)
          seaiceMassU(I,J,bi,bj)=RHOICE*HALF*(
     &          HEFF(i,j,1,bi,bj) + HEFF(i-1,j  ,1,bi,bj) )
          seaiceMassV(I,J,bi,bj)=RHOICE*HALF*(
     &          HEFF(i,j,1,bi,bj) + HEFF(i  ,j-1,1,bi,bj) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( SEAICE_maskRHS ) THEN
C     dynamic masking of areas with no ice
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
           seaiceMaskU(I,J,bi,bj)=AREA(I,J,1,bi,bj)+AREA(I-1,J,1,bi,bj)
           IF ( seaiceMaskU(I,J,bi,bj) .GT. 0. _d 0 ) THEN
            seaiceMaskU(I,J,bi,bj) = 1. _d 0
           ELSE
            seaiceMaskU(I,J,bi,bj) = 0. _d 0
           ENDIF
           seaiceMaskV(I,J,bi,bj)=AREA(I,J,1,bi,bj)+AREA(I,J-1,1,bi,bj)
           IF ( seaiceMaskV(I,J,bi,bj) .GT. 0. _d 0 ) THEN
            seaiceMaskV(I,J,bi,bj) = 1. _d 0
           ELSE
            seaiceMaskV(I,J,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _EXCH_XY_R8( seaiceMaskU, myThid )
       _EXCH_XY_R8( seaiceMaskV, myThid )
      ENDIF

C--   NOW SET UP FORCING FIELDS

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
C first ocean surface stress
          DAIRN(I,J,bi,bj)=RHOAIR*OCEAN_drag
     &         *(2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
          WINDX(I,J,bi,bj)=DAIRN(I,J,bi,bj)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(I,J,bi,bj))*V1)
          WINDY(I,J,bi,bj)=DAIRN(I,J,bi,bj)*
     &         (SIGN(SINWIN, _fCori(I,J,bi,bj))*U1+COSWIN*V1)
C now ice surface stress
          DAIRN(I,J,bi,bj) = RHOAIR*SEAICE_drag*AAA
         ENDDO
        ENDDO
C     now interpolate to U and V points respectively
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          FORCEX0(I,J,bi,bj)=0.5 _d 0 * 
     &         ( DAIRN(I  ,J,bi,bj)*(
     &         COSWIN*uWind(I  ,J,bi,bj)
     &         -SIGN(SINWIN, _fCori(I  ,J,bi,bj))*vWind(I  ,J,bi,bj) )
     &         + DAIRN(I-1,J,bi,bj)*(
     &         COSWIN*uWind(I-1,J,bi,bj)
     &         -SIGN(SINWIN, _fCori(I-1,J,bi,bj))*vWind(I-1,J,bi,bj) )
     &         )
C     interpolate to V point
          FORCEY0(I,J,bi,bj)=0.5 _d 0 * 
     &         ( DAIRN(I,J  ,bi,bj)*(
     &         SIGN(SINWIN, _fCori(I,J  ,bi,bj))*uWind(I,J  ,bi,bj)
     &         +COSWIN*vWind(I,J  ,bi,bj) )
     &         + DAIRN(I,J-1,bi,bj)*(
     &         SIGN(SINWIN, _fCori(I,J-1,bi,bj))*uWind(I,J-1,bi,bj)
     &         +COSWIN*vWind(I,J-1,bi,bj) )
     &         )
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
C--   NOW ADD IN TILT
C--   use actual sea surface height for tilt computations
          FORCEX0(I,J,bi,bj)=FORCEX0(I,J,bi,bj)
     &         -seaiceMassU(I,J,bi,bj)*gravity* _recip_dxC(I,J,bi,bj)
     &         *( etaN(I,J,bi,bj)-etaN(I-1,J,bi,bj) )
     &         
          FORCEY0(I,J,bi,bj)=FORCEY0(I,J,bi,bj)
     &         -seaiceMassV(I,J,bi,bj)*gravity* _recip_dyC(I,J,bi,bj)
     &         *( etaN(I,J,bi,bj)-etaN(I,J-1,bi,bj) )
     &         
C--   NOW SET UP ICE PRESSURE AND VISCOSITIES
          PRESS0(I,J,bi,bj)=PSTAR*HEFF(I,J,1,bi,bj)
     &         *EXP(-20.0 _d 0*(ONE-AREA(I,J,1,bi,bj)))
          ZMAX(I,J,bi,bj)=(5.0 _d +12/(2.0 _d +04))*PRESS0(I,J,bi,bj)
          ZMIN(I,J,bi,bj)=4.0 _d +08
          PRESS0(I,J,bi,bj)=PRESS0(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef SEAICE_ALLOW_DYNAMICS

      IF ( SEAICEuseDYNAMICS ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_ALLOW_EVP
       IF ( SEAICEuseEVP ) THEN
        CALL SEAICE_EVP( myTime, myIter, myThid )
       ELSE
C     do the original VP solver of Zhang and Hibler (1997), ported to
C     a C-grid
#endif /* SEAICE_ALLOW_EVP */
C NOW DO PREDICTOR TIME STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(I,J,2,bi,bj)=uIce(I,J,1,bi,bj)
           vIce(I,J,2,bi,bj)=vIce(I,J,1,bi,bj)
           uIceC(I,J,bi,bj)=uIce(I,J,1,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C NOW LSR SCHEME (ZHANG-J/HIBLER 1997)
       CALL SEAICE_LSR( 1, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
#endif

C NOW DO MODIFIED EULER STEP
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(I,J,1,bi,bj)=HALF*(uIce(I,J,1,bi,bj)+uIce(I,J,2,bi,bj))
           vIce(I,J,1,bi,bj)=HALF*(vIce(I,J,1,bi,bj)+vIce(I,J,2,bi,bj))
           uIceC(I,J,bi,bj)=uIce(I,J,1,bi,bj)
           vIceC(I,J,bi,bj)=vIce(I,J,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       
C NOW LSR SCHEME (ZHANG-J/HIBLER 1997)
       CALL SEAICE_LSR( 2, myThid )
#ifdef SEAICE_ALLOW_EVP
       ENDIF
#endif /* SEAICE_ALLOW_EVP */

      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dwatn = comlev1, key=ikey_dynamics
CADJ STORE uice  = comlev1, key=ikey_dynamics
CADJ STORE vice  = comlev1, key=ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

C Calculate ocean surface stress
      CALL SEAICE_OCEAN_STRESS ( myTime, myIter, myThid )

#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDYNAMICS .AND. SEAICE_clipVelocities) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */
c Put a cap on ice velocity
c limit velocity to 0.40 m s-1 to avoid potential CFL violations
c in open water areas (drift of zero thickness ice)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(i,j,1,bi,bj)=
     &          MAX(MIN(uIce(i,j,1,bi,bj),0.40 _d +00),-0.40 _d +00)
           vIce(i,j,1,bi,bj)=
     &          MAX(MIN(vIce(i,j,1,bi,bj),0.40 _d +00),-0.40 _d +00)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_CGRID */
      RETURN
      END
