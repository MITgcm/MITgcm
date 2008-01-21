C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_dynsolver.F,v 1.30 2008/01/21 17:58:40 heimbach Exp $
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
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

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
      _RL PSTAR
      _RL phiSurf(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

# ifdef ALLOW_AUTODIFF_TAMC
C Following re-initialisation breaks some "artificial" AD dependencies
C incured by IF (DIFFERENT_MULTIPLE ... statement
      PSTAR = SEAICE_strength
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          PRESS0(i,j,bi,bj)=PSTAR*HEFF(i,j,1,bi,bj)
     &         *EXP(-20.0 _d 0*(ONE-AREA(i,j,1,bi,bj)))
          ZMAX(i,j,bi,bj)=(5.0 _d +12/(2.0 _d +04))*PRESS0(i,j,bi,bj)
          ZMIN(i,j,bi,bj)=SEAICE_zetaMin
          PRESS0(i,j,bi,bj)=PRESS0(i,j,bi,bj)*HEFFM(i,j,bi,bj)
          TAUX(i,j,bi,bj)   = 0. _d 0
          TAUY(i,j,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
# endif /* ALLOW_AUTODIFF_TAMC */

      IF (
     &  DIFFERENT_MULTIPLE(SEAICE_deltaTdyn,myTime,SEAICE_deltaTtherm)
     &   ) THEN

C--   FIRST SET UP BASIC CONSTANTS
      PSTAR  = SEAICE_strength

C--   NOW SET UP MASS PER UNIT AREA AND CORIOLIS TERM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          seaiceMassC(I,J,bi,bj)=SEAICE_rhoIce*HEFF(i,j,1,bi,bj)
          seaiceMassU(I,J,bi,bj)=SEAICE_rhoIce*HALF*(
     &          HEFF(i,j,1,bi,bj) + HEFF(i-1,j  ,1,bi,bj) )
          seaiceMassV(I,J,bi,bj)=SEAICE_rhoIce*HALF*(
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
       CALL EXCH_UV_XY_RL( seaiceMaskU, seaiceMaskV, .FALSE., myThid )
      ENDIF

C--   NOW SET UP FORCING FIELDS

C     initialise fields
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          TAUX (I,J,bi,bj)= 0. _d 0
          TAUY (I,J,bi,bj)= 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SEAICE_ALLOW_EVP
          stressDivergenceX(I,J,bi,bj) = 0. _d 0
          stressDivergenceY(I,J,bi,bj) = 0. _d 0
# endif
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
# endif /* ALLOW_AUTODIFF_TAMC */
C--   interface of dynamics with atmopheric forcing fields (wind/stress)
      CALL SEAICE_GET_DYNFORCING (
     I     uIce, vIce,
     O     TAUX, TAUY,
     I     myTime, myIter, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C--   Compute surface pressure at z==0:
C-    use actual sea surface height for tilt computations
        DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            phiSurf(i,j) = Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
          ENDDO
        ENDDO
#ifdef ATMOSPHERIC_LOADING
C-    add atmospheric loading and Sea-Ice loading
        IF ( useRealFreshWaterFlux ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            phiSurf(i,j) = phiSurf(i,j)
     &                   + ( pload(i,j,bi,bj)
     &                      +sIceLoad(i,j,bi,bj)*gravity
     &                     )*recip_rhoConst
           ENDDO
          ENDDO
        ELSE
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            phiSurf(i,j) = phiSurf(i,j)
     &                   + pload(i,j,bi,bj)*recip_rhoConst
           ENDDO
          ENDDO
        ENDIF
#endif /* ATMOSPHERIC_LOADING */
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
C--   now add in wind stress ant tilt
          FORCEX0(I,J,bi,bj)=TAUX(I,J,bi,bj)
     &         -seaiceMassU(I,J,bi,bj)*_recip_dxC(I,J,bi,bj)
     &         *( phiSurf(i,j)-phiSurf(i-1,j) )
          FORCEY0(I,J,bi,bj)=TAUY(I,J,bi,bj)
     &         -seaiceMassV(I,J,bi,bj)* _recip_dyC(I,J,bi,bj)
     &         *( phiSurf(i,j)-phiSurf(i,j-1) )
         ENDDO
        ENDDO

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C--   now set up ice pressure and viscosities
          PRESS0(I,J,bi,bj)=PSTAR*HEFF(I,J,1,bi,bj)
     &         *EXP(-20.0 _d 0*(ONE-AREA(I,J,1,bi,bj)))
          ZMAX(I,J,bi,bj)  = (5. _d +12/(2. _d +04))*PRESS0(I,J,bi,bj)
          ZMIN(I,J,bi,bj)  = SEAICE_zetaMin
          PRESS0(I,J,bi,bj)= PRESS0(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef SEAICE_ALLOW_DYNAMICS

      IF ( SEAICEuseDYNAMICS ) THEN

#ifdef SEAICE_ALLOW_EVP
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics
CADJ STORE vice = comlev1, key=ikey_dynamics
# endif /* ALLOW_AUTODIFF_TAMC */
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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uicec = comlev1, key=ikey_dynamics
CADJ STORE vicec = comlev1, key=ikey_dynamics
# ifdef SEAICE_ALLOW_EVP
cphCADJ STORE uice = comlev1, key=ikey_dynamics
cphCADJ STORE vice = comlev1, key=ikey_dynamics
# endif
#endif
C NOW LSR SCHEME (ZHANG-J/HIBLER 1997)
       CALL SEAICE_LSR( 1, myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE uice = comlev1, key=ikey_dynamics
cphCADJ STORE vice = comlev1, key=ikey_dynamics
cphCADJ STORE uicec = comlev1, key=ikey_dynamics
cphCADJ STORE vicec = comlev1, key=ikey_dynamics
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
       CALL SEAICE_LSR( 2, myTime, myIter, myThid )
#ifdef SEAICE_ALLOW_EVP
       ENDIF
#endif /* SEAICE_ALLOW_EVP */

C End of IF (SEAICEuseDYNAMICS ...
      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */

C End of IF (DIFFERENT_MULTIPLE ...
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
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
