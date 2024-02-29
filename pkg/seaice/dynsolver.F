#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CStartOfInterface
      SUBROUTINE DYNSOLVER( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE DYNSOLVER                                     |
C     | o B-grid version of ice dynamics using LSR solver        |
C     |   Zhang and Hibler, JGR, 102, 8691-8702, 1997            |
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif
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

#ifdef SEAICE_BGRID_DYNAMICS

#ifdef EXPLICIT_SSH_SLOPE
#include "SURFACE.h"
      _RL phiSurf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL RHOICE, RHOAIR
      _RL COSWIN
      _RS SINWIN
      _RL ECCEN, ECM2, PSTAR, AAA
      _RL U1, V1

      _RL COR_ICE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

C--   FIRST SET UP BASIC CONSTANTS
      RHOICE=SEAICE_rhoIce
      RHOAIR=SEAICE_rhoAir
      ECCEN=SEAICE_eccen
      ECM2=ONE/(ECCEN**2)
      PSTAR=SEAICE_strength

C--   introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   Compute proxy for geostrophic velocity,
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy+1
         DO i=0,sNx+1
          GWATX(i,j,bi,bj)=HALF*(uVel(i,j,KGEO(i,j,bi,bj),bi,bj)
     &                         +uVel(i,j-1,KGEO(i,j,bi,bj),bi,bj))
          GWATY(i,j,bi,bj)=HALF*(vVel(i,j,KGEO(i,j,bi,bj),bi,bj)
     &                         +vVel(i-1,j,KGEO(i,j,bi,bj),bi,bj))
#ifdef SEAICE_DEBUG
c          write(*,'(2i4,2i2,f7.1,7f12.3)')
c     &     ,i,j,bi,bj,UVM(I,J,bi,bj)
c     &     ,GWATX(I,J,bi,bj),GWATY(I,J,bi,bj)
c     &     ,uVel(i+1,j,3,bi,bj),uVel(i+1,j+1,3,bi,bj)
c     &     ,vVel(i,j+1,3,bi,bj),vVel(i+1,j+1,3,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   NOW SET UP MASS PER UNIT AREA AND CORIOLIS TERM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          COR_ICE(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
          AMASS(i,j,bi,bj)=RHOICE*QUART*(
     &          HEFF(i,j  ,bi,bj) + HEFF(i-1,j  ,bi,bj)
     &         +HEFF(i,j-1,bi,bj) + HEFF(i-1,j-1,bi,bj) )
          COR_ICE(i,j,bi,bj)=AMASS(i,j,bi,bj) * _fCoriG(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   NOW SET UP FORCING FIELDS

C--   Wind stress is computed on South-West B-grid U/V
C     locations from wind on tracer locations
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          U1=QUART*(UWIND(i-1,j-1,bi,bj)+UWIND(i-1,j,bi,bj)
     &             +UWIND(i  ,j-1,bi,bj)+UWIND(i  ,j,bi,bj))
          V1=QUART*(VWIND(i-1,j-1,bi,bj)+VWIND(i-1,j,bi,bj)
     &             +VWIND(i  ,j-1,bi,bj)+VWIND(i  ,j,bi,bj))
          AAA=U1**2+V1**2
          IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
             AAA=SEAICE_EPS
          ELSE
             AAA=SQRT(AAA)
          ENDIF
C first ocean surface stress
          DAIRN(i,j,bi,bj)=RHOAIR*OCEAN_drag
     &         *(2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
          WINDX(i,j,bi,bj)=DAIRN(i,j,bi,bj)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(i,j,bi,bj))*V1)
          WINDY(i,j,bi,bj)=DAIRN(i,j,bi,bj)*
     &         (SIGN(SINWIN, _fCori(i,j,bi,bj))*U1+COSWIN*V1)

C now ice surface stress
          IF ( YC(i,j,bi,bj) .LT. ZERO ) THEN
           DAIRN(i,j,bi,bj) =
     &          RHOAIR*(SEAICE_drag_south*AAA*AREA(i,j,bi,bj)
     &          +OCEAN_drag*(2.70 _d 0+0.142 _d 0*AAA
     &          +0.0764 _d 0*AAA*AAA)*(ONE-AREA(i,j,bi,bj)))
          ELSE
           DAIRN(i,j,bi,bj) =
     &          RHOAIR*(SEAICE_drag*AAA*AREA(i,j,bi,bj)
     &          +OCEAN_drag*(2.70 _d 0+0.142 _d 0*AAA
     &          +0.0764 _d 0*AAA*AAA)*(ONE-AREA(i,j,bi,bj)))
          ENDIF
          FORCEX(i,j,bi,bj)=DAIRN(i,j,bi,bj)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(i,j,bi,bj))*V1)
          FORCEY(i,j,bi,bj)=DAIRN(i,j,bi,bj)*
     &         (SIGN(SINWIN, _fCori(i,j,bi,bj))*U1+COSWIN*V1)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef EXPLICIT_SSH_SLOPE
C--   Compute surface pressure at z==0:
C-    use actual sea surface height for tilt computations
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            phiSurf(i,j) = Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
          ENDDO
        ENDDO
#ifdef ATMOSPHERIC_LOADING
C-    add atmospheric loading and Sea-Ice loading
        IF ( useRealFreshWaterFlux ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            phiSurf(i,j) = phiSurf(i,j)
     &                   + ( pload(i,j,bi,bj)
     &                      +sIceLoad(i,j,bi,bj)*gravity*sIceLoadFac
     &                     )*recip_rhoConst
           ENDDO
          ENDDO
        ELSE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            phiSurf(i,j) = phiSurf(i,j)
     &                   + pload(i,j,bi,bj)*recip_rhoConst
           ENDDO
          ENDDO
        ENDIF
#endif /* ATMOSPHERIC_LOADING */
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
C--   NOW ADD IN TILT
          FORCEX(i,j,bi,bj)=FORCEX(i,j,bi,bj)
     &      -AMASS(i,j,bi,bj)
     &         *( (phiSurf(i, j )-phiSurf(i-1, j ))*SIMaskU(i, j ,bi,bj)
     &           +(phiSurf(i,j-1)-phiSurf(i-1,j-1))*SIMaskV(i,j-1,bi,bj)
     &          )*HALF*_recip_dxV(i,j,bi,bj)
          FORCEY(i,j,bi,bj)=FORCEY(i,j,bi,bj)
     &      -AMASS(i,j,bi,bj)
     &         *( (phiSurf( i ,j)-phiSurf( i ,j-1))*SIMaskV( i ,j,bi,bj)
     &           +(phiSurf(i-1,j)-phiSurf(i-1,j-1))*SIMaskV(i-1,j,bi,bj)
     &          )*HALF*_recip_dyU(i,j,bi,bj)
C NOW KEEP FORCEX0
          FORCEX0(i,j,bi,bj)=FORCEX(i,j,bi,bj)
          FORCEY0(i,j,bi,bj)=FORCEY(i,j,bi,bj)
         ENDDO
        ENDDO
#endif /* EXPLICIT_SSH_SLOPE */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifndef EXPLICIT_SSH_SLOPE
C--   NOW ADD IN TILT
          FORCEX(i,j,bi,bj)=FORCEX(i,j,bi,bj)
     &         -COR_ICE(i,j,bi,bj)*GWATY(i,j,bi,bj)
          FORCEY(i,j,bi,bj)=FORCEY(i,j,bi,bj)
     &         +COR_ICE(i,j,bi,bj)*GWATX(i,j,bi,bj)
C NOW KEEP FORCEX0
          FORCEX0(i,j,bi,bj)=FORCEX(i,j,bi,bj)
          FORCEY0(i,j,bi,bj)=FORCEY(i,j,bi,bj)
#endif /* EXPLICIT_SSH_SLOPE */
C--   NOW SET UP ICE PRESSURE AND VISCOSITIES
          PRESS0(i,j,bi,bj)=PSTAR*HEFF(i,j,bi,bj)
     &         *EXP(-SEAICE_cStar*(ONE-AREA(i,j,bi,bj)))
CML          SEAICE_zMax(I,J,bi,bj)=(5.0 _d +12/(2.0 _d +04))*PRESS0(I,J,bi,bj)
          SEAICE_zMax(i,j,bi,bj)=SEAICE_zetaMaxFac*PRESS0(i,j,bi,bj)
CML          SEAICE_zMin(I,J,bi,bj)=4.0 _d +08
          SEAICE_zMin(i,j,bi,bj)=SEAICE_zetaMin
          PRESS0(i,j,bi,bj)=PRESS0(i,j,bi,bj)*HEFFM(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( SEAICEuseDYNAMICS ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

crg what about ETA,ZETA

crg later c$taf loop = iteration uice,vice

cdm c$taf store uice,vice = comlev1_seaice_ds,
cdm c$taf&                key = kii + (ikey_dynamics-1)
C NOW DO PREDICTOR TIME STEP
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICENM1(i,j,bi,bj)=UICE(i,j,bi,bj)
          VICENM1(i,j,bi,bj)=VICE(i,j,bi,bj)
          UICEB(i,j,bi,bj)=UICE(i,j,bi,bj)
          VICEB(i,j,bi,bj)=VICE(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C NOW LSR SCHEME (ZHANG-J/HIBLER 1997)
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
      CALL LSR( 1, myThid )
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics

C NOW DO MODIFIED EULER STEP
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(i,j,bi,bj)=HALF*(UICE(i,j,bi,bj)+UICENM1(i,j,bi,bj))
          VICE(i,j,bi,bj)=HALF*(VICE(i,j,bi,bj)+VICENM1(i,j,bi,bj))
          UICEB(i,j,bi,bj)=UICE(i,j,bi,bj)
          VICEB(i,j,bi,bj)=VICE(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C NOW LSR SCHEME (ZHANG-J/HIBLER 1997)
      CALL LSR( 2, myThid )

cdm c$taf store uice,vice = comlev1, key=ikey_dynamics

      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(zeta   ,'SIzeta  ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(eta    ,'SIeta   ',0,1 ,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(press  ,'SIpress ',0,1 ,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C Calculate ocean surface stress
      CALL OSTRES ( COR_ICE, myThid )

#ifdef SEAICE_ALLOW_CLIPVELS
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
#ifdef SEAICE_DEBUG
c          write(*,'(2i4,2i2,f7.1,7f12.3)')
c     &      i,j,bi,bj,UVM(I,J,bi,bj),amass(i,j,bi,bj)
c     &     ,gwatx(I,J,bi,bj),gwaty(i,j,bi,bj)
c     &     ,forcex(I,J,bi,bj),forcey(i,j,bi,bj)
c     &     ,uice(i,j,bi,bj)
c     &     ,vice(i,j,bi,bj)
#endif /* SEAICE_DEBUG */
          UICE(i,j,bi,bj)=min(UICE(i,j,bi,bj),0.40 _d +00)
          VICE(i,j,bi,bj)=min(VICE(i,j,bi,bj),0.40 _d +00)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(i,j,bi,bj)=max(UICE(i,j,bi,bj),-0.40 _d +00)
          VICE(i,j,bi,bj)=max(VICE(i,j,bi,bj),-0.40 _d +00)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      ENDIF
#endif /* SEAICE_ALLOW_CLIPVELS */
#endif /* SEAICE_BGRID_DYNAMICS */

      RETURN
      END
