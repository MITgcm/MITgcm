C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_advdiff.F,v 1.50 2011/06/07 03:58:23 gforget Exp $
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
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

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
#ifdef ALLOW_SITRACER
      INTEGER iTr
      _RL SItrEFF   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpscal1, tmpscal2
#ifdef ALLOW_SITRACER_ADVCAP
      _RL SItrPrev  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_SITRACER_DIAG
      _RL DIAGarray (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#endif
#ifdef SEAICE_AGE
      INTEGER iTracer
#endif

      _RL uc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fldNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL iceFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gFld      (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
          vc(i,j,bi,bj)=.5 _d 0*(VICE(i,j,bi,bj)+VICE(i+1,j,bi,bj))
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
C-    compute cell areas used by all tracers
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xA(i,j,bi,bj) = _dyG(i,j,bi,bj)*_maskW(i,j,ks,bi,bj)
          yA(i,j,bi,bj) = _dxG(i,j,bi,bj)*_maskS(i,j,ks,bi,bj)
         ENDDO
        ENDDO
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
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE hsalt   = comlev1, key = ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
      IF ( SEAICEmultiDimAdvection ) THEN
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

C-    Calculate "volume transports" through tracer cell faces.
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uTrans(i,j) = uc(i,j,bi,bj)*xA(i,j,bi,bj)
          vTrans(i,j) = vc(i,j,bi,bj)*yA(i,j,bi,bj)
         ENDDO
        ENDDO

C--   Effective Thickness (Volume)
        IF ( SEAICEadvHeff ) THEN
          CALL SEAICE_ADVECTION(
     I         GAD_HEFF, SEAICEadvSchHeff,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HEFF(1-OLx,1-OLy,bi,bj), recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
         IF ( SEAICEdiffKhHeff .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_HEFF, SEAICEdiffKhHeff, ONE,
     I         HEFF(1-OLx,1-OLy,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
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
     I         GAD_AREA, SEAICEadvSchArea,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, AREA(1-OLx,1-OLy,bi,bj), recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
         IF ( SEAICEdiffKhArea .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_AREA, SEAICEdiffKhArea, ONE,
     I         AREA(1-OLx,1-OLy,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
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
     I         GAD_SNOW, SEAICEadvSchSnow,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HSNOW(1-OLx,1-OLy,bi,bj), recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
         IF ( SEAICEdiffKhSnow .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_SNOW, SEAICEdiffKhSnow, ONE,
     I         HSNOW(1-OLx,1-OLy,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
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

#ifdef SEAICE_VARIABLE_SALINITY
C--   Effective Sea Ice Salinity (Mass of salt)
        IF ( SEAICEadvSalt ) THEN
          CALL SEAICE_ADVECTION(
     I         GAD_SALT, SEAICEadvSchSalt,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HSALT(1-OLx,1-OLy,bi,bj), recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
         IF ( SEAICEdiffKhSalt .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_SALT, SEAICEdiffKhSalt, ONE,
     I         HSALT(1-OLx,1-OLy,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
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
#endif /* SEAICE_VARIABLE_SALINITY */

#ifdef ALLOW_SITRACER
C--   Sea Ice Tracers
        IF ( SEAICEadvHEFF ) THEN
         DO iTr = 1, SItrMaxNum
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
C--   scale to effective value
            SItrEFF(i,j,bi,bj) = HEFFM(i,j,bi,bj) *
     &          SItracer(i,j,bi,bj,iTr) * HEFFNM1(i,j,bi,bj)
#ifdef ALLOW_SITRACER_ADVCAP
C--   store previous value for spurious maxima treament
            SItrPrev(i,j,bi,bj)=SItracer(i,j,bi,bj,iTr)
#endif
#ifdef ALLOW_SITRACER_DIAG
            diagArray(I,J,2+(iTr-1)*5) = SItrEFF(i,j,bi,bj)
#endif
           ENDDO
          ENDDO
C--   compute advective tendency 
          CALL SEAICE_ADVECTION(
     I         GAD_SITR+iTr-1, SEAICEadvSchHEFF,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, SItrEFF(1-OLx,1-OLy,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
          IF ( SEAICEdiffKhHeff .GT. 0. _d 0 ) THEN
C--   add diffusive tendency
          CALL SEAICE_DIFFUSION(
     I         GAD_SITR+iTr-1, SEAICEdiffKhHEFF, ONE,
     I         SItrEFF(1-OLx,1-OLy,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
          ENDIF
C--   apply tendency
          DO j=1,sNy
           DO i=1,sNx
            SItrEFF(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &        SItrEFF(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j) )
           ENDDO
          ENDDO
C--   scale back to actual value, or move effective value to ocean bucket
          DO j=1,sNy
           DO i=1,sNx
            if (HEFF(I,J,bi,bj).GE.siEps) then
            SItracer(i,j,bi,bj,iTr)=SItrEFF(i,j,bi,bj)/HEFF(I,J,bi,bj)
            SItrBucket(i,j,bi,bj,iTr)=0. _d 0
            else
            SItracer(i,j,bi,bj,iTr)=0. _d 0
            SItrBucket(i,j,bi,bj,iTr)=SItrEFF(i,j,bi,bj)
            endif
#ifdef ALLOW_SITRACER_ADVCAP
c hack to try avoid 'spontaneous generation' of maxima, which supposedly would
c occur less frequently if we advected SItr with uXheff instead SItrXheff with u
             tmpscal1=max(SItrPrev(i,j,bi,bj),
     &       SItrPrev(i+1,j,bi,bj),SItrPrev(i-1,j,bi,bj),
     &       SItrPrev(i,j+1,bi,bj),SItrPrev(i,j-1,bi,bj))
             tmpscal2=MAX(ZERO,SItracer(i,j,bi,bj,iTr)-tmpscal1)
             SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)-tmpscal2
             SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &           +tmpscal2*HEFF(I,J,bi,bj)
#endif
c           treat case of potential negative value
            if (HEFF(I,J,bi,bj).GE.siEps) then
              tmpscal1=MIN(0. _d 0,SItracer(i,j,bi,bj,iTr))
              SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)-tmpscal1
              SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &                                 +HEFF(I,J,bi,bj)*tmpscal1
            endif
#ifdef ALLOW_SITRACER_DIAG
      diagArray(I,J,1+(iTr-1)*5)= - SItrBucket(i,j,bi,bj,iTr)
     &  *HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm*SEAICE_rhoIce
      tmpscal1= ( HEFF(I,J,bi,bj)*SItracer(i,j,bi,bj,iTr) 
     &  + SItrBucket(i,j,bi,bj,iTr) )*HEFFM(I,J,bi,bj)
      diagArray(I,J,2+(iTr-1)*5)= tmpscal1-diagArray(I,J,2+(iTr-1)*5)
      diagArray(I,J,3+(iTr-1)*5)=HEFFM(i,j,bi,bj) * 
     &  SEAICE_deltaTtherm * gFld(i,j)
#endif
           ENDDO
          ENDDO
C--
         ENDDO
#ifdef ALLOW_SITRACER_DIAG
      CALL DIAGNOSTICS_FILL(DIAGarray,'UDIAG2  ',0,Nr,2,bi,bj,myThid)
#endif
        ENDIF
#endif /* ALLOW_SITRACER */

#ifdef SEAICE_AGE
C--   Sea Ice Age
        IF ( SEAICEadvAge ) THEN
         DO iTracer = 1, SEAICE_num
C--
          CALL SEAICE_ADVECTION(
     I         GAD_SITR+SItrMaxNum+iTracer-1, SEAICEadvSchAge,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, IceAgeTr(1-OLx,1-OLy,bi,bj,iTracer),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
          IF ( SEAICEdiffKhAge .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I         GAD_SITR+SItrMaxNum+iTracer-1, SEAICEdiffKhAge, ONE,
     I         IceAgeTr(1-OLx,1-OLy,bi,bj,iTracer), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
          ENDIF
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            IceAgeTr(i,j,bi,bj,iTracer) = HEFFM(i,j,bi,bj) * (
     &          IceAgeTr(i,j,bi,bj,iTracer)
     &          + SEAICE_deltaTtherm * gFld(i,j)
     &          )
           ENDDO
          ENDDO
C--
         ENDDO
        ENDIF
#endif /* SEAICE_AGE */

C---   end bi,bj loops
       ENDDO
      ENDDO

      ELSE
C--   if not multiDimAdvection

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc   = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE vc   = comlev1, key = ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

       IF ( SEAICEadvHEff ) THEN
        CALL ADVECT( uc, vc, hEff, hEffNm1, HEFFM, myThid )
        IF ( SEAICEdiffKhHeff .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION(
     I           GAD_HEFF, SEAICEdiffKhHeff, SEAICE_deltaTtherm,
     I           hEffNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           HEFF(1-OLx,1-OLy,bi,bj),
     I           bi, bj, myTime, myIter, myThid )
          ENDDO
         ENDDO
        ENDIF
       ENDIF
       IF ( SEAICEadvArea ) THEN
        CALL ADVECT( uc, vc, area, areaNm1, HEFFM, myThid )
        IF ( SEAICEdiffKhArea .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION(
     I           GAD_AREA, SEAICEdiffKhArea, SEAICE_deltaTtherm,
     I           AreaNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           Area(1-OLx,1-OLy,bi,bj),
     I           bi, bj, myTime, myIter, myThid )
          ENDDO
         ENDDO
        ENDIF
       ENDIF
       IF ( SEAICEadvSnow ) THEN
        CALL ADVECT( uc, vc, HSNOW, fldNm1, HEFFM, myThid )
        IF ( SEAICEdiffKhSnow .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION(
     I           GAD_SNOW, SEAICEdiffKhSnow, SEAICE_deltaTtherm,
     I           fldNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           HSNOW(1-OLx,1-OLy,bi,bj),
     I           bi, bj, myTime, myIter, myThid )
          ENDDO
         ENDDO
        ENDIF
       ENDIF

#ifdef SEAICE_VARIABLE_SALINITY
       IF ( SEAICEadvSalt ) THEN
        CALL ADVECT( uc, vc, HSALT, fldNm1, HEFFM, myThid )
        IF ( SEAICEdiffKhSalt .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION(
     I           GAD_SALT, SEAICEdiffKhSalt, SEAICE_deltaTtherm,
     I           fldNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           HSALT(1-OLx,1-OLy,bi,bj),
     I           bi, bj, myTime, myIter, myThid )
          ENDDO
         ENDDO
        ENDIF
       ENDIF
#endif /* SEAICE_VARIABLE_SALINITY */

#ifdef SEAICE_AGE
       IF ( SEAICEadvAge ) THEN
        DO iTracer = 1, SEAICE_num
         CALL ADVECT( uc, vc, iceAgeTr(1-Olx,1-Oly,1,1,iTracer),
     &                fldNm1, HEFFM, myThid )
         IF ( SEAICEdiffKhAge .GT. 0. _d 0 ) THEN
C--   Add tendency due to diffusion
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION( GAD_SITR+SItrMaxNum+iTracer-1,
     I           SEAICEdiffKhAge, SEAICE_deltaTtherm,
     I           fldNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           iceAgeTr(1-Olx,1-Oly,bi,bj,iTracer),
     I           bi, bj, myTime, myIter, myThid )
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDIF
#endif /* SEAICE_AGE */

C--   end if multiDimAdvection
      ENDIF

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
#ifdef SEAICE_AGE
C     avoid ridging of sea ice age (at this point ridged ice means AREA > 1)
           DO iTracer = 1, SEAICE_num
            IceAgeTr(i,j,bi,bj,iTracer) = IceAgeTr(i,j,bi,bj,iTracer)
     &          / MAX(ONE,AREA(i,j,bi,bj))
           ENDDO
#endif /* SEAICE_AGE */
           AREA(I,J,bi,bj) = MIN(ONE,AREA(I,J,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef SEAICE_AGE
C     Sources and sinks for sea ice age (otherwise added in seaice_growth)
       IF ( .TRUE. ) THEN
       DO iTracer = 1, SEAICE_num
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
cph(
cph-- why is this code still here?
cph-- probably wrong for thickness-weighted age(?)
            IF ( AREA(i,j,bi,bj) .GT. 0.15 ) THEN
             IceAgeTr(i,j,bi,bj,iTracer) = IceAgeTr(i,j,bi,bj,iTracer)
     &            + AREA(i,j,bi,bj) * SEAICE_deltaTtherm
            ELSE
             IceAgeTr(i,j,bi,bj,iTracer) = ZERO
            ENDIF
cph)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDIF
#endif /* SEAICE_AGE */
      ENDIF

      RETURN
      END
