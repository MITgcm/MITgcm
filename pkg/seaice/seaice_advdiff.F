C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_advdiff.F,v 1.40 2010/06/27 20:17:00 jmc Exp $
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

C-    MPI+MTH: apply exch (sure with exch1) only to array in common block
      COMMON / SEAICE_ADVDIFF_LOCAL / uc, vc
      _RL uc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fldNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL iceFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gFld      (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_heff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ks = 1

C--   make a local copy of the velocities for compatibility with B-grid
C--   alternatively interpolate to C-points if necessary
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_CGRID
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uc(i,j,bi,bj)=UICE(i,j,bi,bj)
          vc(i,j,bi,bj)=VICE(i,j,bi,bj)
         ENDDO
        ENDDO
#else /* not SEAICE_CGRID = BGRID */
C average seaice velocity to C-grid
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          uc(i,j,bi,bj)=.5 _d 0*(UICE(i,j,bi,bj)+UICE(i,j+1,bi,bj))
          vc(i,j,bi,bj)=.5 _d 0*(VICE(i,j,bi,bj)+VICE(I+1,J,bi,bj))
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


#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area   = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE heff   = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE heffm  = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE hsnow  = comlev1, key = ikey_dynamics, kind=isbyte
# ifdef SEAICE_SALINITY
CADJ STORE hsalt   = comlev1, key = ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
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
CADJ STORE uc     = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE vc     = comlev1, key = ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C---   loops on tile indices bi,bj

#ifdef ALLOW_AUTODIFF_TAMC
C     Initialise for TAF
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
c         iceFld(i,j)     = 0. _d 0
          gFld(i,j)       = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFNM1(i,j,bi,bj) = HEFF(i,j,bi,bj)
          AREANM1(i,j,bi,bj) = AREA(i,j,bi,bj)
          recip_heff(i,j)    = 1. _d 0
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
         CALL SEAICE_ADVECTION(
     I        GAD_HEFF, SEAICEadvSchHeff,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, HEFF(1-OLx,1-OLy,bi,bj), recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_HEFF,
     I         HEFF(1-OLx,1-OLy,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           HEFF(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          HEFF(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

C--   Fractional area
        IF ( SEAICEadvArea ) THEN
         CALL SEAICE_ADVECTION(
     I        GAD_AREA, SEAICEadvSchArea,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, AREA(1-OLx,1-OLy,bi,bj), recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_AREA,
     I         AREA(1-OLx,1-OLy,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           AREA(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          AREA(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

C--   Effective Snow Thickness (Volume)
        IF ( SEAICEadvSnow ) THEN
         CALL SEAICE_ADVECTION(
     I        GAD_SNOW, SEAICEadvSchSnow,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, HSNOW(1-OLx,1-OLy,bi,bj), recip_heff,
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
         CALL SEAICE_ADVECTION(
     I        GAD_SALT, SEAICEadvSchSalt,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, HSALT(1-OLx,1-OLy,bi,bj), recip_heff,
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

#ifdef SEAICE_AGE
C--   Sea Ice Age
        IF ( SEAICEadvAge ) THEN
         CALL SEAICE_ADVECTION(
     I        GAD_AGE, SEAICEadvSchAge,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, IceAge(1-OLx,1-OLy,bi,bj), recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
         IF ( diff1 .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_AGE,
     I         IceAge(1-OLx,1-OLy,bi,bj), HEFFM, xA, yA,
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
         ENDIF
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           IceAge(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          IceAge(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_AGE */

C---   end bi,bj loops
       ENDDO
      ENDDO

#else
      STOP 'SEAICEmultiDimAdvection not yet implemented for adjoint'
#endif /* ndef ALLOW_AUTODIFF_TAMC */
      ELSE
C--   if not multiDimAdvection

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc   = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE vc   = comlev1, key = ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

       IF ( SEAICEadvHEff ) THEN
        CALL ADVECT( uc, vc, hEff, hEffNm1, HEFFM, myThid )
       ENDIF
       IF ( SEAICEadvArea ) THEN
        CALL ADVECT( uc, vc, area, areaNm1, HEFFM, myThid )
       ENDIF
       IF ( SEAICEadvSnow ) THEN
        CALL ADVECT( uc, vc, HSNOW, fldNm1, HEFFM, myThid )
       ENDIF

#ifdef SEAICE_SALINITY
       IF ( SEAICEadvSalt ) THEN
        CALL ADVECT( uc, vc, HSALT, fldNm1, HEFFM, myThid )
       ENDIF
#endif /* SEAICE_SALINITY */

#ifdef SEAICE_AGE
       IF ( SEAICEadvAge ) THEN
        CALL ADVECT( uc, vc, iceAge, fldNm1, HEFFM, myThid )
       ENDIF
#endif /* SEAICE_AGE */

C--   end if multiDimAdvection
      ENDIF

#ifdef SEAICE_AGE
C     avoid ridging of sea ice age (at this point ridged ice means AREA > 1)
      IF ( .TRUE. ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           IceAge(I,J,bi,bj) = IceAge(I,J,bi,bj)
     &          / MAX(ONE,AREA(I,J,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* SEAICE_AGE */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE AREA   = comlev1, key = ikey_dynamics, kind=isbyte
#endif
      IF ( .NOT. usePW79thermodynamics ) THEN
C     Hiblers "ridging function": Do it now if not in seaice_growth
C     in principle we should add a "real" ridging function here (or
C     somewhere after doing the advection)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           AREA(I,J,bi,bj) = MIN(ONE,AREA(I,J,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
