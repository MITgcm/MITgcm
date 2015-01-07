C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_dynsolver.F,v 1.57 2015/01/07 13:27:10 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_DYNSOLVER
C     !INTERFACE:
      SUBROUTINE SEAICE_DYNSOLVER( myTime, myIter, myThid )

C     *==========================================================*
C     | SUBROUTINE SEAICE_DYNSOLVER
C     | o Ice dynamics using LSR solver
C     |   Zhang and Hibler,   JGR, 102, 8691-8702, 1997
C     |   or EVP explicit solver by Hunke and Dukowicz, JPO 27,
C     |   1849-1867 (1997)
C     *==========================================================*
C     | written by Martin Losch, March 2006
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime     :: Simulation time
C     myIter     :: Simulation timestep number
C     myThid     :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_CGRID

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
      _RL PSTAR
      _RL phiSurf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  mask_uice, mask_vice

# ifdef ALLOW_AUTODIFF_TAMC
C Following re-initialisation breaks some "artificial" AD dependencies
C incured by IF (DIFFERENT_MULTIPLE ... statement
      PSTAR = SEAICE_strength
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          PRESS0(i,j,bi,bj) = PSTAR*HEFF(i,j,bi,bj)
     &         *EXP(-SEAICE_cStar*(ONE-AREA(i,j,bi,bj)))
          ZMAX(I,J,bi,bj)   = SEAICE_zetaMaxFac*PRESS0(I,J,bi,bj)
          ZMIN(i,j,bi,bj)   = SEAICE_zetaMin
          PRESS0(i,j,bi,bj) = PRESS0(i,j,bi,bj)*HEFFM(i,j,bi,bj)
          TAUX(i,j,bi,bj)   = 0. _d 0
          TAUY(i,j,bi,bj)   = 0. _d 0
#ifdef SEAICE_ALLOW_FREEDRIFT
          uice_fd(i,j,bi,bj)= 0. _d 0
          vice_fd(i,j,bi,bj)= 0. _d 0
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
CADJ STORE uice    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uicenm1 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vicenm1 = comlev1, key=ikey_dynamics, kind=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */

      IF (
     &  DIFFERENT_MULTIPLE(SEAICE_deltaTdyn,myTime,SEAICE_deltaTtherm)
     &   ) THEN

# ifdef ALLOW_AUTODIFF_TAMC
# ifdef SEAICE_ALLOW_EVP
CADJ STORE press0 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE zmax   = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# endif /* ALLOW_AUTODIFF_TAMC */

C--   NOW SET UP MASS PER UNIT AREA AND CORIOLIS TERM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          seaiceMassC(I,J,bi,bj)=SEAICE_rhoIce*HEFF(i,j,bi,bj)
          seaiceMassU(I,J,bi,bj)=SEAICE_rhoIce*HALF*(
     &          HEFF(i,j,bi,bj) + HEFF(i-1,j  ,bi,bj) )
          seaiceMassV(I,J,bi,bj)=SEAICE_rhoIce*HALF*(
     &          HEFF(i,j,bi,bj) + HEFF(i  ,j-1,bi,bj) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifndef ALLOW_AUTODIFF_TAMC
      IF ( SEAICE_maskRHS ) THEN
C     dynamic masking of areas with no ice
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           seaiceMaskU(I,J,bi,bj)=AREA(i,j,bi,bj)+AREA(I-1,J,bi,bj)
           mask_uice=HEFFM(i,j,bi,bj)+HEFFM(i-1,j  ,bi,bj)
           IF ( (seaiceMaskU(I,J,bi,bj) .GT. 0. _d 0) .AND.
     &         (mask_uice .GT. 1.5 _d 0) ) THEN
            seaiceMaskU(I,J,bi,bj) = 1. _d 0
           ELSE
            seaiceMaskU(I,J,bi,bj) = 0. _d 0
           ENDIF
           seaiceMaskV(I,J,bi,bj)=AREA(i,j,bi,bj)+AREA(I,J-1,bi,bj)
           mask_vice=HEFFM(i,j,bi,bj)+HEFFM(i  ,j-1,bi,bj)
           IF ( (seaiceMaskV(I,J,bi,bj) .GT. 0. _d 0) .AND.
     &         (mask_vice .GT. 1.5 _d 0) ) THEN
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
#endif /* ndef ALLOW_AUTODIFF_TAMC */

C--   NOW SET UP FORCING FIELDS

C     initialise fields
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
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
CADJ STORE uice = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice = comlev1, key=ikey_dynamics, kind=isbyte
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
     &                      +sIceLoad(i,j,bi,bj)*gravity
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
C--   basic forcing by wind stress
          FORCEX0(I,J,bi,bj)=TAUX(I,J,bi,bj)
          FORCEY0(I,J,bi,bj)=TAUY(I,J,bi,bj)
         ENDDO
        ENDDO

        IF ( SEAICEuseTILT ) then
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
C--   now add in tilt
          FORCEX0(I,J,bi,bj)=FORCEX0(I,J,bi,bj)
     &         -seaiceMassU(I,J,bi,bj)*_recip_dxC(I,J,bi,bj)
     &         *( phiSurf(i,j)-phiSurf(i-1,j) )
          FORCEY0(I,J,bi,bj)=FORCEY0(I,J,bi,bj)
     &         -seaiceMassV(I,J,bi,bj)* _recip_dyC(I,J,bi,bj)
     &         *( phiSurf(i,j)-phiSurf(i,j-1) )
         ENDDO
        ENDDO
        ENDIF

        CALL SEAICE_CALC_ICE_STRENGTH( bi, bj, myTime, myIter, myThid )

       ENDDO
      ENDDO

#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDYNAMICS ) THEN

#ifdef SEAICE_ALLOW_FREEDRIFT
       IF ( SEAICEuseFREEDRIFT .OR. SEAICEuseEVP
     &                         .OR. LSR_mixIniGuess.EQ.0 ) THEN
        CALL SEAICE_FREEDRIFT( myTime, myIter, myThid )
       ENDIF
       IF ( SEAICEuseFREEDRIFT ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uIce(i,j,bi,bj) = uIce_fd(i,j,bi,bj)
            vIce(i,j,bi,bj) = vIce_fd(i,j,bi,bj)
            stressDivergenceX(i,j,bi,bj) = 0. _d 0
            stressDivergenceY(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */

#ifdef ALLOW_OBCS
       IF ( useOBCS ) THEN
         CALL OBCS_APPLY_UVICE( uIce, vIce, myThid )
       ENDIF
#endif /* ALLOW_OBCS */

#ifdef SEAICE_ALLOW_EVP
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uicenm1 = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vicenm1 = comlev1, key=ikey_dynamics, kind=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
       IF ( SEAICEuseEVP ) THEN
C     Elastic-Viscous-Plastic solver, following Hunke (2001)
        CALL SEAICE_EVP( myTime, myIter, myThid )
       ENDIF
#endif /* SEAICE_ALLOW_EVP */

#ifdef SEAICE_ALLOW_JFNK
       IF ( SEAICEuseJFNK ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
        STOP 'Adjoint does not work with JFNK solver.'
# else
C     Jacobian-free Newton Krylov solver (Lemieux et al. 2010, 2012)
        CALL SEAICE_JFNK( myTime, myIter, myThid )
# endif /*  ALLOW_AUTODIFF_TAMC */
       ENDIF
#endif /* SEAICE_ALLOW_JFNK */

#if defined(SEAICE_ALLOW_EVP) || defined(SEAICE_ALLOW_JFNK) \
                              || defined(SEAICE_ALLOW_FREEDRIFT)
       IF ( .NOT.SEAICEuseFREEDRIFT .AND. .NOT.SEAICEuseEVP
     &      .AND. .NOT.SEAICEuseJFNK ) THEN
#endif /* SEAICE_ALLOW_EVP or SEAICE_ALLOW_FREEDRIFT */
C     LSR scheme (Zhang-J/Hibler 1997), ported to a C-grid
          CALL SEAICE_LSR( myTime, myIter, myThid )
#if defined(SEAICE_ALLOW_EVP) || defined(SEAICE_ALLOW_JFNK) \
                              || defined(SEAICE_ALLOW_FREEDRIFT)
       ENDIF
#endif /* SEAICE_ALLOW_EVP or SEAICE_ALLOW_FREEDRIFT */

C End of IF (SEAICEuseDYNAMICS ...
      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */

C End of IF (DIFFERENT_MULTIPLE ...
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE stressDivergenceX  = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
CADJ STORE stressDivergenceY  = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
CADJ STORE DWATN  = comlev1, key=ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C Calculate ocean surface stress
      CALL SEAICE_OCEAN_STRESS ( myTime, myIter, myThid )

#ifdef SEAICE_ALLOW_DYNAMICS
#ifdef SEAICE_ALLOW_CLIPVELS
      IF ( SEAICEuseDYNAMICS .AND. SEAICE_clipVelocities) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uice = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice = comlev1, key=ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
c Put a cap on ice velocity
c limit velocity to 0.40 m s-1 to avoid potential CFL violations
c in open water areas (drift of zero thickness ice)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uIce(i,j,bi,bj)=
     &          MAX(MIN(uIce(i,j,bi,bj),0.40 _d +00),-0.40 _d +00)
           vIce(i,j,bi,bj)=
     &          MAX(MIN(vIce(i,j,bi,bj),0.40 _d +00),-0.40 _d +00)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* SEAICE_ALLOW_CLIPVELS */
#endif /* SEAICE_ALLOW_DYNAMICS */

#endif /* SEAICE_CGRID */
      RETURN
      END
