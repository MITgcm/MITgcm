#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SEAICE_ADVDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ADVDIFF(
     U                  uc, vc,
     I                  myTime, myIter, myThid )

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
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT/OUTPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     uc/vc     :: current ice velocity on C-grid;
C               :: C-Grid : Input only ; B-grid : Output only
C     myTime    :: current time in simulation
C     myIter    :: current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL uc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
C     it        :: Loop counter for ice thickness categories
C     uTrans    :: volume transport, x direction
C     vTrans    :: volume transport, y direction
C     afx       :: horizontal advective flux, x direction
C     afy       :: horizontal advective flux, y direction
C     gFld      :: tendency of seaice field
C     xA,yA     :: "areas" of X and Y face of tracer cells
      INTEGER i, j, bi, bj
#ifdef SEAICE_ITD
      INTEGER it
#endif /* SEAICE_ITD */
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_SITRACER
      _RL hEffNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL areaNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iTr, SEAICEadvSchSItr
      _RL SEAICEdiffKhSItr
      _RL SItrExt   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpscal1, tmpscal2
# ifdef ALLOW_SITRACER_ADVCAP
      _RL SItrPrev  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif
# ifdef ALLOW_SITRACER_DEBUG_DIAG
      _RL DIAGarray (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
# endif
#endif /* ALLOW_SITRACER */
      _RL fldNm1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL recip_heff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   make a local copy of the velocities for compatibility with B-grid
C--   alternatively interpolate to C-points if necessary
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifndef SEAICE_CGRID /* not SEAICE_CGRID = BGRID */
C--   hack to ensure backward compatibility:
C     average B-grid seaice velocity to C-grid
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          uc(i,j,bi,bj)=.5 _d 0*(UICE(i,j,bi,bj)+UICE(i,j+1,bi,bj))
          vc(i,j,bi,bj)=.5 _d 0*(VICE(i,j,bi,bj)+VICE(i+1,j,bi,bj))
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
C-    compute cell areas used by all tracers
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j,bi,bj) = _dyG(i,j,bi,bj)*SIMaskU(i,j,bi,bj)
          yA(i,j,bi,bj) = _dxG(i,j,bi,bj)*SIMaskV(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifndef SEAICE_CGRID
C     Do we need this? I am afraid so.
      CALL EXCH_UV_XY_RL(uc,vc,.TRUE.,myThid)
#endif /* not SEAICE_CGRID */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc   = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE vc   = comlev1, key = ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

      IF ( SEAICEmultiDimAdvection ) THEN
#ifdef ALLOW_GENERIC_ADVDIFF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C---   loops on tile indices bi,bj

#ifdef ALLOW_AUTODIFF_TAMC
C     Initialise for TAF
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gFld(i,j)       = 0. _d 0
         ENDDO
        ENDDO
C
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        itmpkey = (act1 + 1) + act2*max1
     &                       + act3*max1*max2
     &                       + act4*max1*max2*max3
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key=itmpkey, kind=isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key=itmpkey, kind=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, key=itmpkey, kind=isbyte
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE hsalt(:,:,bi,bj) = comlev1_bibj, key=itmpkey, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef ALLOW_SITRACER
          hEffNm1(i,j,bi,bj) = HEFF(i,j,bi,bj)
          areaNm1(i,j,bi,bj) = AREA(i,j,bi,bj)
#endif
          recip_heff(i,j)    = 1. _d 0
         ENDDO
        ENDDO

C-    Calculate "volume transports" through tracer cell faces.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uTrans(i,j) = uc(i,j,bi,bj)*xA(i,j,bi,bj)
          vTrans(i,j) = vc(i,j,bi,bj)*yA(i,j,bi,bj)
         ENDDO
        ENDDO

#ifdef SEAICE_ITD
C--   Effective Thickness (Volume)
        IF ( SEAICEadvHeff ) THEN
         DO it=1,SEAICE_multDim
          CALL SEAICE_ADVECTION(
     I         GAD_HEFF, SEAICEadvSchHeff,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HEFFITD(1-OLx,1-OLy,it,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
C-    Add tendency due to diffusion
          IF ( SEAICEdiffKhHeff .GT. 0. _d 0 )
     &         CALL SEAICE_DIFFUSION(
     I         GAD_HEFF, SEAICEdiffKhHeff, ONE,
     I         HEFFITD(1-OLx,1-OLy,it,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            HEFFITD(i,j,it,bi,bj) = HEFFM(i,j,bi,bj) * (
     &           HEFFITD(i,j,it,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &           )
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C--   Fractional area
        IF ( SEAICEadvArea ) THEN
         DO it=1,SEAICE_multDim
          CALL SEAICE_ADVECTION(
     I         GAD_AREA, SEAICEadvSchArea,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, AREAITD(1-OLx,1-OLy,it,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
C-    Add tendency due to diffusion
          IF ( SEAICEdiffKhArea .GT. 0. _d 0 )
     &         CALL SEAICE_DIFFUSION(
     I         GAD_AREA, SEAICEdiffKhArea, ONE,
     I         AREAITD(1-OLx,1-OLy,it,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            AREAITD(i,j,it,bi,bj) = HEFFM(i,j,bi,bj) * (
     &           AREAITD(i,j,it,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &           )
           ENDDO
          ENDDO
         ENDDO
C     open water fraction needs to be advected for the ridging scheme
         CALL SEAICE_ADVECTION(
     I        GAD_AREA, SEAICEadvSchArea,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, opnWtrFrac(1-OLx,1-OLy,bi,bj), recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
C--   Add tendency due to diffusion
         IF ( SEAICEdiffKhArea .GT. 0. _d 0 )
     &        CALL SEAICE_DIFFUSION(
     I        GAD_AREA, SEAICEdiffKhArea, ONE,
     I        opnWtrFrac(1-OLx,1-OLy,bi,bj), HEFFM,
     I        xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U        gFld,
     I        bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           opnWtrFrac(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          opnWtrFrac(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

C--   Effective Snow Thickness (Volume)
        IF ( SEAICEadvSnow ) THEN
         DO it=1,SEAICE_multDim
          CALL SEAICE_ADVECTION(
     I         GAD_SNOW, SEAICEadvSchSnow,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HSNOWITD(1-OLx,1-OLy,it,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
C--   Add tendency due to diffusion
          IF ( SEAICEdiffKhSnow .GT. 0. _d 0 )
     &         CALL SEAICE_DIFFUSION(
     I         GAD_SNOW, SEAICEdiffKhSnow, ONE,
     I         HSNOWITD(1-OLx,1-OLy,it,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            HSNOWITD(i,j,it,bi,bj) = HEFFM(i,j,bi,bj) * (
     &           HSNOWITD(i,j,it,bi,bj) + SEAICE_deltaTtherm*gFld(i,j)
     &           )
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C     update mean ice thickness HEFF and total ice concentration AREA
C     to match single category values
C     (necessary here because updated HEFF is used below for SItracer)
        CALL SEAICE_ITD_SUM(bi, bj, myTime, myIter, myThid)

#else /* not SEAICE_ITD */
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
#endif /* SEAICE_ITD */

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
        DO iTr = 1, SItrNumInUse
        IF ( (SEAICEadvHEFF.AND.(SItrMate(iTr).EQ.'HEFF')).OR.
     &       (SEAICEadvAREA.AND.(SItrMate(iTr).EQ.'AREA')) ) THEN
C--   scale to effective value
         IF (SItrMate(iTr).EQ.'HEFF') THEN
          SEAICEadvSchSItr=SEAICEadvSchHEFF
          SEAICEdiffKhSItr=SEAICEdiffKhHEFF
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            SItrExt(i,j,bi,bj) = HEFFM(i,j,bi,bj) *
     &          SItracer(i,j,bi,bj,iTr) * hEffNm1(i,j,bi,bj)
           ENDDO
          ENDDO
c TAF?   ELSEIF (SItrMate(iTr).EQ.'AREA') THEN
         ELSE
          SEAICEadvSchSItr=SEAICEadvSchAREA
          SEAICEdiffKhSItr=SEAICEdiffKhAREA
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            SItrExt(i,j,bi,bj) = HEFFM(i,j,bi,bj) *
     &          SItracer(i,j,bi,bj,iTr) * areaNm1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
C--   store a couple things
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
#ifdef ALLOW_SITRACER_ADVCAP
C--   store previous value for spurious maxima treament
            SItrPrev(i,j,bi,bj)=SItracer(i,j,bi,bj,iTr)
#endif
#ifdef ALLOW_SITRACER_DEBUG_DIAG
            diagArray(I,J,2+(iTr-1)*5) = SItrExt(i,j,bi,bj)
#endif
           ENDDO
          ENDDO
C--   compute advective tendency
          CALL SEAICE_ADVECTION(
     I         GAD_SITR+iTr-1, SEAICEadvSchSItr,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, SItrExt(1-OLx,1-OLy,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
          IF ( SEAICEdiffKhHeff .GT. 0. _d 0 ) THEN
C--   add diffusive tendency
          CALL SEAICE_DIFFUSION(
     I         GAD_SITR+iTr-1, SEAICEdiffKhSItr, ONE,
     I         SItrExt(1-OLx,1-OLy,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
          ENDIF
C--   apply tendency
          DO j=1,sNy
           DO i=1,sNx
            SItrExt(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &        SItrExt(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j) )
           ENDDO
          ENDDO
C--   scale back to actual value, or move effective value to ocean bucket
         IF (SItrMate(iTr).EQ.'HEFF') THEN
          DO j=1,sNy
           DO i=1,sNx
            if (HEFF(I,J,bi,bj).GE.siEps) then
            SItracer(i,j,bi,bj,iTr)=SItrExt(i,j,bi,bj)/HEFF(I,J,bi,bj)
            SItrBucket(i,j,bi,bj,iTr)=0. _d 0
            else
            SItracer(i,j,bi,bj,iTr)=0. _d 0
            SItrBucket(i,j,bi,bj,iTr)=SItrExt(i,j,bi,bj)
            endif
#ifdef ALLOW_SITRACER_ADVCAP
C hack to try avoid 'spontaneous generation' of maxima, which supposedly would
C occur less frequently if we advected SItr with uXheff instead SItrXheff with u
             tmpscal1=max(SItrPrev(i,j,bi,bj),
     &       SItrPrev(i+1,j,bi,bj),SItrPrev(i-1,j,bi,bj),
     &       SItrPrev(i,j+1,bi,bj),SItrPrev(i,j-1,bi,bj))
             tmpscal2=MAX(ZERO,SItracer(i,j,bi,bj,iTr)-tmpscal1)
             SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)-tmpscal2
             SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &           +tmpscal2*HEFF(I,J,bi,bj)
#endif
C           treat case of potential negative value
            if (HEFF(I,J,bi,bj).GE.siEps) then
              tmpscal1=MIN(0. _d 0,SItracer(i,j,bi,bj,iTr))
              SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)-tmpscal1
              SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &                                 +HEFF(I,J,bi,bj)*tmpscal1
            endif
#ifdef ALLOW_SITRACER_DEBUG_DIAG
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
c TAF?   ELSEIF (SItrMate(iTr).EQ.'AREA') THEN
         ELSE
          DO j=1,sNy
           DO i=1,sNx
            if (AREA(I,J,bi,bj).GE.SEAICE_area_floor) then
            SItracer(i,j,bi,bj,iTr)=SItrExt(i,j,bi,bj)/AREA(I,J,bi,bj)
            else
            SItracer(i,j,bi,bj,iTr)=0. _d 0
            endif
            SItrBucket(i,j,bi,bj,iTr)=0. _d 0
#ifdef ALLOW_SITRACER_ADVCAP
             tmpscal1=max(SItrPrev(i,j,bi,bj),
     &       SItrPrev(i+1,j,bi,bj),SItrPrev(i-1,j,bi,bj),
     &       SItrPrev(i,j+1,bi,bj),SItrPrev(i,j-1,bi,bj))
             tmpscal2=MAX(ZERO,SItracer(i,j,bi,bj,iTr)-tmpscal1)
             SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)-tmpscal2
#endif
C           treat case of potential negative value
            if (AREA(I,J,bi,bj).GE.SEAICE_area_floor) then
              tmpscal1=MIN(0. _d 0,SItracer(i,j,bi,bj,iTr))
              SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)-tmpscal1
            endif
#ifdef ALLOW_SITRACER_DEBUG_DIAG
      diagArray(I,J,1+(iTr-1)*5)= 0. _d 0
      diagArray(I,J,2+(iTr-1)*5)= - diagArray(I,J,2+(iTr-1)*5)
     & + AREA(I,J,bi,bj)*SItracer(i,j,bi,bj,iTr)*HEFFM(I,J,bi,bj)
      diagArray(I,J,3+(iTr-1)*5)=HEFFM(i,j,bi,bj) *
     &  SEAICE_deltaTtherm * gFld(i,j)
#endif
           ENDDO
          ENDDO
         ENDIF
C--
         ENDIF
        ENDDO
#ifdef ALLOW_SITRACER_DEBUG_DIAG
c     CALL DIAGNOSTICS_FILL(DIAGarray,'UDIAG2  ',0,Nr,2,bi,bj,myThid)
#endif
#endif /* ALLOW_SITRACER */

C---   end bi,bj loops
       ENDDO
      ENDDO

#else /* not ALLOW_GENERIC_ADVDIFF */
      WRITE(msgBuf,'(2A)')
     &  'SEAICE_ADVDIFF: cannot use SEAICEmultiDimAdvection',
     &  ' without pkg/generic_advdiff'
      CALL PRINT_ERROR( msgBuf , myThid )
      WRITE(msgBuf,'(2A)') 'SEAICE_ADVDIFF: ',
     &  'Re-compile with pkg "generic_advdiff" in packages.conf'
      CALL PRINT_ERROR( msgBuf , myThid )
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R SEAICE_ADVDIFF'
#endif /* ALLOW_GENERIC_ADVDIFF */
      ELSE
C--   if not multiDimAdvection
#ifdef SEAICE_ITD
C     just for safety
      WRITE(msgBuf,'(2A)') 'SEAICE_ADVDIFF: ',
     &  'ITD with SEAICEmultiDimAdvection=.False. is not allowed,'
      CALL PRINT_ERROR( msgBuf , myThid )
      WRITE(msgBuf,'(2A)') 'SEAICE_ADVDIFF: ',
     &  'use a multidimensional advection scheme (in data.seaice)'
      CALL PRINT_ERROR( msgBuf , myThid )
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R SEAICE_ADVDIFF'
#endif /* SEAICE_ITD */

       IF ( SEAICEadvHEff ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff = comlev1, key = ikey_dynamics, kind=isbyte
#endif
        CALL ADVECT( uc, vc, hEff, fldNm1, HEFFM, myThid )
        IF ( SEAICEdiffKhHeff .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION(
     I           GAD_HEFF, SEAICEdiffKhHeff, SEAICE_deltaTtherm,
     I           fldNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           HEFF(1-OLx,1-OLy,bi,bj),
     I           bi, bj, myTime, myIter, myThid )
          ENDDO
         ENDDO
        ENDIF
       ENDIF
       IF ( SEAICEadvArea ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area = comlev1, key = ikey_dynamics, kind=isbyte
#endif
        CALL ADVECT( uc, vc, area, fldNm1, HEFFM, myThid )
        IF ( SEAICEdiffKhArea .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            CALL SEAICE_DIFFUSION(
     I           GAD_AREA, SEAICEdiffKhArea, SEAICE_deltaTtherm,
     I           fldNm1(1-OLx,1-OLy,bi,bj), HEFFM,
     I           xA(1-OLx,1-OLy,bi,bj), yA(1-OLx,1-OLy,bi,bj),
     U           Area(1-OLx,1-OLy,bi,bj),
     I           bi, bj, myTime, myIter, myThid )
          ENDDO
         ENDDO
        ENDIF
       ENDIF
       IF ( SEAICEadvSnow ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow = comlev1, key = ikey_dynamics, kind=isbyte
#endif
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt = comlev1, key = ikey_dynamics, kind=isbyte
#endif
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

C--   end if multiDimAdvection
      ENDIF

      RETURN
      END
