C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_advdiff.F,v 1.26 2007/10/26 18:27:25 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C !ROUTINE: SEAICE_ADVDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ADVDIFF(
     I     myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ADVDIFF
C     | o driver for different advection routines
C     |   calls an adaption of gad_advection to call different
C     |   advection routines of pkg/generic_advdiff
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
C     UICE/VICE :: ice velocity
C     HEFF      :: scalar field to be advected
C     HEFFM     :: mask for scalar field
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SEAICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
C     ks        :: surface level index
C     uc/vc     :: current ice velocity on C-grid
C     uTrans    :: volume transport, x direction
C     vTrans    :: volume transport, y direction
C     iceFld    :: copy of seaice field
C     afx       :: horizontal advective flux, x direction
C     afy       :: horizontal advective flux, y direction
C     gFld      :: tendency of seaice field
C     xA,yA     :: "areas" of X and Y face of tracer cells
      INTEGER i, j, bi, bj
      INTEGER ks
      LOGICAL SEAICEmultiDimAdvection

      _RL uc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gFld      (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL fld3d     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_heff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ks = 1

C--   Get rid of the time dimension for velocities and interpolate
C--   to C-points if necessary
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_CGRID
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uc(i,j,bi,bj)=UICE(i,j,1,bi,bj)
          vc(i,j,bi,bj)=VICE(i,j,1,bi,bj)
         ENDDO
        ENDDO
#else /* not SEAICE_CGRID = BGRID */
C average seaice velocity to c-grid
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          uc(i,j,bi,bj)=.5 _d 0*(UICE(i,j,1,bi,bj)+UICE(i,j+1,1,bi,bj))
          vc(i,j,bi,bj)=.5 _d 0*(VICE(i,j,1,bi,bj)+VICE(I+1,J,1,bi,bj))
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
       ENDDO
      ENDDO

#ifndef SEAICE_CGRID
C     Do we need this? I am afraid so.
      CALL EXCH_UV_XY_RL(uc,vc,.TRUE.,myThid)
#endif /* not SEAICE_CGRID */

      SEAICEmultidimadvection = .TRUE.
      IF ( SEAICEadvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.SEAICEadvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.SEAICEadvScheme.EQ.ENUM_CENTERED_4TH ) THEN
       SEAICEmultiDimAdvection = .FALSE.
      ENDIF


      IF ( SEAICEmultiDimAdvection ) THEN
#ifndef ALLOW_AUTODIFF_TAMC
C     This has to be done to comply with the time stepping in advect.F:
C     Making sure that the following routines see the different
C     time levels correctly
C     At the end of the routine ADVECT,
C     timelevel 1 is updated with advection contribution
C                 and diffusion contribution
C                 (which was computed in DIFFUS on timelevel 3)
C     timelevel 2 is the previous timelevel 1
C     timelevel 3 is the total diffusion tendency * deltaT
C                 (empty if no diffusion)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc     = comlev1, key = ikey_dynamics
CADJ STORE vc     = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C---   loops on tile indices bi,bj

#ifdef ALLOW_AUTODIFF_TAMC
C     Initialise for TAF
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          iceFld(i,j)     = 0. _d 0
          gFld(i,j)       = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(i,j,3,bi,bj) = 0. _d 0
          HEFF(i,j,2,bi,bj) = HEFF(i,j,1,bi,bj)
          AREA(i,j,3,bi,bj) = 0. _d 0
          AREA(i,j,2,bi,bj) = AREA(i,j,1,bi,bj)
          recip_heff(i,j)   = 1. _d 0
         ENDDO
        ENDDO

C-    first compute cell areas used by all tracers
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xA(i,j) = _dyG(i,j,bi,bj)*_maskW(i,j,ks,bi,bj)
          yA(i,j) = _dxG(i,j,bi,bj)*_maskS(i,j,ks,bi,bj)
         ENDDO
        ENDDO
C-    Calculate "volume transports" through tracer cell faces.
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uTrans(i,j) = uc(i,j,bi,bj)*xA(i,j)
          vTrans(i,j) = vc(i,j,bi,bj)*yA(i,j)
         ENDDO
        ENDDO

C--   Effective Thickness (Volume)
        IF ( SEAICEadvHeff ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceFld(i,j) = HEFF(i,j,1,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_ADVECTION(
     I        GAD_HEFF, SEAICEadvSchHeff,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_HEFF,
     I         HEFF(1-OLx,1-OLy,1,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           HEFF(i,j,1,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          HEFF(i,j,1,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

C--   Fractional area
        IF ( SEAICEadvArea ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceFld(i,j) = AREA(i,j,1,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_ADVECTION(
     I        GAD_AREA, SEAICEadvSchArea,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_AREA,
     I         AREA(1-OLx,1-OLy,1,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           AREA(i,j,1,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          AREA(i,j,1,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

C--   Effective Snow Thickness (Volume)
        IF ( SEAICEadvSnow ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceFld(i,j) = HSNOW(i,j,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_ADVECTION(
     I        GAD_SNOW, SEAICEadvSchSnow,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_SNOW,
     I         HSNOW(1-OLx,1-OLy,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           HSNOW(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          HSNOW(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

#ifdef SEAICE_SALINITY
C--   Effective Sea Ice Salinity (Mass of salt)
        IF ( SEAICEadvSalt ) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceFld(i,j) = HSALT(i,j,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_ADVECTION(
     I        GAD_SALT, SEAICEadvSchSalt,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_SALT,
     I         HSALT(1-OLx,1-OLy,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           HSALT(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          HSALT(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_SALINITY */

C---   end bi,bj loops
       ENDDO
      ENDDO

#else
      STOP 'SEAICEmultiDimAdvection not yet implemented for adjoint'
#endif /* ndef ALLOW_AUTODIFF_TAMC */
      ELSE
C--   if not multiDimAdvection

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc   = comlev1, key = ikey_dynamics
CADJ STORE vc   = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

       IF ( SEAICEadvHEff ) CALL ADVECT( uc, vc, HEFF, HEFFM, myThid )
       IF ( SEAICEadvArea ) CALL ADVECT( uc, vc, AREA, HEFFM, myThid )

C     another fudge: copy 2D field HSNOW to 3D field fld3d to be able to
C     use ADVECT. Works only with LAD=2 (Backward Euler scheme) and NOT
C     with LAD=1 (Leapfrog).
       IF ( SEAICEadvSnow ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            fld3d(I,J,3,bi,bj) = 0. _d 0
            fld3d(I,J,2,bi,bj) = HSNOW(I,J,bi,bj)
            fld3d(I,J,1,bi,bj) = HSNOW(I,J,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL ADVECT( uc, vc, fld3d, HEFFM, myThid )
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSNOW(I,J,bi,bj) = fld3d(I,J,1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

#ifdef SEAICE_SALINITY
C     another fudge: copy 2D field HSALT to 3D field fld3d to be able to
C     use ADVECT. Works only with LAD=2 (Backward Euler scheme) and NOT
C     with LAD=1 (Leapfrog).
       IF ( SEAICEadvSalt ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            fld3d(I,J,3,bi,bj) = 0. _d 0
            fld3d(I,J,2,bi,bj) = HSALT(I,J,bi,bj)
            fld3d(I,J,1,bi,bj) = HSALT(I,J,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL ADVECT( uc, vc, fld3d, HEFFM, myThid )
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSALT(I,J,bi,bj) = fld3d(I,J,1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* SEAICE_SALINITY */

#ifdef ALLOW_OBCS
#ifndef DISABLE_SEAICE_OBCS
       IF ( useOBCS ) THEN
C-     Apply open boundary conditions
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          IF ( SEAICEadvHeff ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = HEFF(i,j,1,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_HEFF( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             HEFF(i,j,1,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
          IF ( SEAICEadvArea ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = AREA(i,j,1,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_AREA( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             AREA(i,j,1,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
#ifdef SEAICE_SALINITY
          IF ( SEAICEadvSALT ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = HSALT(i,j,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_HSALT( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             HSALT(i,j,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
#endif /* SEAICE_SALINITY */
          IF ( SEAICEadvSNOW ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = HSNOW(i,j,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_HSNOW( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             HSNOW(i,j,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
#endif /* DISABLE_SEAICE_OBCS */
#endif /* ALLOW_OBCS */

C--   end if multiDimAdvection
      ENDIF
C--   end if SEAICEuseDynamics

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE AREA   = comlev1, key = ikey_dynamics
#endif
      IF ( .NOT. usePW79thermodynamics ) THEN
C     Hiblers "ridging function": Do it now if not in seaice_growth
C     in principle we should add a "real" ridging function here (or 
C     somewhere after doing the advection)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           AREA(I,J,1,bi,bj) = MIN(ONE, AREA(I,J,1,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_SEAICE */

      RETURN
      END
