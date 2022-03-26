#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_STEP_TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_STEP_TEMP(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_STEP_TEMP
C     | o Step Forward Surface and SeaIce Temperature.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C- input:
C     bi,bj   :: tile indices
C   iMin,iMax :: computation domain: 1rst index range
C   jMin,jMax :: computation domain: 2nd  index range
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number
C     myThid  :: my Thread Id number
C-- Modify fluxes hold in commom blocks
C- input:
C     icFlxSW :: (Inp) short-wave heat flux (+=down): downward comp. only
C- output
C     icFlxSW :: (Out) net SW flux into ocean (+=down)
C     icFlxAtm:: net flux of energy from the atmosphere [W/m2] (+=down)
C     icFrwAtm:: evaporation to the atmosphere (kg/m2/s) (>0 if evaporate)
C--
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     tFrzOce   :: sea-water freezing temperature [oC] (function of S)
C     dTsrf     :: surf. temp adjusment: Ts^n+1 - Ts^n
C     tmpFlx    :: dummy array for surface fluxes and derivative vs Tsurf
C Note: dTsrf & tmpFlx are not used here; just allocate enough space for dTsrf.
      INTEGER i,j
      _RL tFrzOce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTsrf  (1:sNx,1:sNy)
C     some compilers complain about the wrong dimensions of this field
C     so it has to be defined with overlaps
      _RL tmpFlx (1:sNx,1:sNy,0:2)
      _RL opFrac, icFrac
      LOGICAL dBugFlag

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey
#endif
C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,1P4E14.6)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act1 + 1) + act2*max1
     &                     + act3*max1*max2
     &                     + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C     Initialise some local arrays
      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
        tFrzOce(i,j)  = 0. _d 0
       ENDDO
      ENDDO
      DO j = 1, sNy
       DO i = 1, sNx
        dTsrf(i,j)    = 0. _d 0
        tmpFlx(i,j,0) = 0. _d 0
        tmpFlx(i,j,1) = 0. _d 0
        tmpFlx(i,j,2) = 0. _d 0
       ENDDO
      ENDDO

      dBugFlag = debugLevel.GE.debLevC
C-    Initialise flxAtm,evpAtm
      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
        icFlxAtm(i,j,bi,bj) = 0.
        icFrwAtm(i,j,bi,bj) = 0.
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tsrf(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
#endif
c     IF ( fluidIsWater ) THEN
       CALL THSICE_ALBEDO(
     I          bi, bj, siLo, siHi, sjLo, sjHi,
     I          iMin,iMax, jMin,jMax,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     I          snowAge(siLo,sjLo,bi,bj),
     O          siceAlb(siLo,sjLo,bi,bj), icAlbNIR(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.1 : ice-covered fraction ;
C     Solve for surface and ice temperature (implicitly) ; compute surf. fluxes
C-------

#ifdef ALLOW_AUTODIFF_TAMC
C     avoid calling s/r thsice_albedosolve4temp in adjoint routine again
CADJ STORE siceAlb (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE icAlbNIR(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE icFlxSW (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
#endif

#ifdef ALLOW_DBUG_THSICE
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
          IF ( dBug(i,j,bi,bj) ) THEN
           WRITE(6,'(A,2I4,2I2)') 'ThSI_STEP_T: i,j=',i,j,bi,bj
           WRITE(6,1010) 'ThSI_STEP_T: iceMask, hIc, hSn, Tsf  =',
     &                   iceMask(i,j,bi,bj), iceHeight(i,j,bi,bj),
     &                   snowHeight(i,j,bi,bj), Tsrf(i,j,bi,bj)
          ENDIF
         ENDIF
        ENDDO
       ENDDO
#endif
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
C-      surface net SW flux:
          icFlxSW(i,j,bi,bj) = icFlxSW(i,j,bi,bj)
     &                       *(1. _d 0 - siceAlb(i,j,bi,bj))
          tFrzOce(i,j) = -mu_Tf*sOceMxL(i,j,bi,bj)
         ELSE
          tFrzOce(i,j) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Qice1   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Qice2   (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE sHeating(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE Tsrf    (:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
#endif
       CALL THSICE_SOLVE4TEMP(
     I          bi, bj,
     I          iMin,iMax, jMin,jMax, dBugFlag,
     I          useBulkForce, useEXF,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), tFrzOce, tmpFlx,
     U          icFlxSW(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     U          Qice1(siLo,sjLo,bi,bj), Qice2(siLo,sjLo,bi,bj),
     O          Tice1(siLo,sjLo,bi,bj), Tice2(siLo,sjLo,bi,bj), dTsrf,
     O          sHeating(siLo,sjLo,bi,bj), flxCndBt(siLo,sjLo,bi,bj),
     O          icFlxAtm(siLo,sjLo,bi,bj), icFrwAtm(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
C     avoid calling s/r thsice_solve4temp in adjoint routine again
CADJ STORE icFlxSW(:,:,bi,bj) = comlev1_bibj, key=ticekey, kind=isbyte
#endif
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
          icFrac  = iceMask(i,j,bi,bj)
          opFrac = 1. _d 0 - icFrac
C--    Update Fluxes :
          Qsw(i,j,bi,bj) = opFrac*Qsw(i,j,bi,bj)
     &                   - icFrac*icFlxSW(i,j,bi,bj)
         ENDIF
        ENDDO
       ENDDO
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
