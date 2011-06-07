C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_impl_temp.F,v 1.8 2011/06/07 22:26:37 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

C     !ROUTINE: THSICE_IMPL_TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_IMPL_TEMP(
     I                netSW, sFlx,
     O                dTsurf,
     I                bi, bj, myTime, myIter, myThid)
C     *==========================================================*
C     | S/R  THSICE_IMPL_TEMP
C     | o Calculate sea-ice and surface temp. implicitly
C     *==========================================================*
C     | o return surface fluxes for atmosphere boundary layer
C     |  physics (and therefore called within atmospheric physics)
C     *==========================================================*

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
      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     netSW   :: net Short Wave surf. flux (+=down) [W/m2]
C     sFlx    :: surf. heat flux (+=down) except SW, function of surf. temp Ts:
C                0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C     dTsurf  :: surf. temp adjusment: Ts^n+1 - Ts^n
C     bi,bj   :: Tile index
C     myIter  :: iteration counter for this thread
C     myTime  :: time counter for this thread
C     myThid  :: thread number for this instance of the routine.
      _RL netSW  (sNx,sNy)
      _RL sFlx   (sNx,sNy,0:2)
      _RL dTsurf (sNx,sNy)
      INTEGER bi,bj
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     tFrzOce  :: sea-water freezing temperature [oC] (function of S)
C     dTsrf    :: surf. temp adjusment: Ts^n+1 - Ts^n
      INTEGER i,j
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL tFrzOce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL dTsrf  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL dBugFlag

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,1P4E14.6)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy
      dBugFlag = debugLevel.GE.debLevC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.1 : ice-covered fraction ;
C     Solve for surface and ice temperature (implicitly) ; compute surf. fluxes
C-------
      DO j = jMin, jMax
       DO i = iMin, iMax
        icFlxSW(i,j,bi,bj) = netSW(i,j)
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
          tFrzOce(i,j) = -mu_Tf*sOceMxL(i,j,bi,bj)
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) THEN
           WRITE(6,'(A,2I4,2I2)') 'ThSI_IMPL_T: i,j=',i,j,bi,bj
           WRITE(6,1010) 'ThSI_IMPL_T:-0- iceMask,hIc,hSn,Tsf=',
     &                   iceMask(i,j,bi,bj), iceHeight(i,j,bi,bj),
     &                   snowHeight(i,j,bi,bj), Tsrf(i,j,bi,bj)
           WRITE(6,1010) 'ThSI_IMPL_T:-0- Tice(1,2),Qice(1,2)=',
     &           Tice1(i,j,bi,bj), Tice2(i,j,bi,bj),
     &           Qice1(i,j,bi,bj), Qice2(i,j,bi,bj)
          ENDIF
#endif
        ENDIF
       ENDDO
      ENDDO

      CALL THSICE_SOLVE4TEMP(
     I          bi, bj,
     I          iMin,iMax, jMin,jMax, dBugFlag, .FALSE.,.FALSE.,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), tFrzOce, sFlx,
     U          icFlxSW(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     U          Qice1(siLo,sjLo,bi,bj), Qice2(siLo,sjLo,bi,bj),
     O          Tice1(siLo,sjLo,bi,bj), Tice2(siLo,sjLo,bi,bj), dTsurf,
     O          sHeating(siLo,sjLo,bi,bj), flxCndBt(siLo,sjLo,bi,bj),
     O          icFlxAtm(siLo,sjLo,bi,bj), icFrwAtm(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
