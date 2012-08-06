C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_balance_frw.F,v 1.2 2012/08/06 16:56:59 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_BALANCE_FRW
C     !INTERFACE:
      SUBROUTINE THSICE_BALANCE_FRW(
     I                          iMin, iMax, jMin, jMax,
     I                          prcAtm, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_BALANCE_FRW
C     | o Correct ocean fresh-water forcing for global imbalance
C     |   of Atmos+Land fresh-water flux
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     iMin,iMax :: computation domain: 1rst index range
C     jMin,jMax :: computation domain: 2nd  index range
C     prcAtm    :: precip (+RunOff) from Atmos+Land
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_BALANCE_FLUXES
C     !LOCAL VARIABLES:
C     bi,bj     :: Tile indices
C     i, j      :: loop indices
      INTEGER bi,bj
      INTEGER i, j
      _RL sumPrc, sumTilePrc(nSx,nSy)
      _RL sumFrW, sumTileFrW(nSx,nSy)
      _RL tmpFac, tmpVar

C--   Calculate and global-mean precip (+RunOff)
C     and global-mean imbalance of net Atmos Fresh-Water flux
      IF ( thSIceBalanceAtmFW.NE.0 ) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          sumTilePrc(bi,bj) = 0. _d 0
          sumTileFrW(bi,bj) = 0. _d 0
          DO j = 1,sNy
           DO i = 1,sNx
            sumTilePrc(bi,bj) = sumTilePrc(bi,bj)
     &                        + MAX( prcAtm(i,j,bi,bj), zeroRL )
     &                         *rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
            sumTileFrW(bi,bj) = sumTileFrW(bi,bj)
     &                        + icFrwAtm(i,j,bi,bj)
     &                         *rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        sumPrc = 0. _d 0
        IF ( thSIceBalanceAtmFW.EQ.2 )
     &  CALL GLOBAL_SUM_TILE_RL( sumTilePrc, sumPrc, myThid )
        CALL GLOBAL_SUM_TILE_RL( sumTileFrW, sumFrW, myThid )

        IF ( globalArea.GT.0. _d 0 ) THEN
          sumPrc = sumPrc / globalArea
          sumFrW = sumFrW / globalArea
        ENDIF

C-    save amount of correction (for diagnostics)
        _BEGIN_MASTER(myThid)
        adjustFrW = -sumFrW
        _END_MASTER(myThid)

      ENDIF

      IF     ( thSIceBalanceAtmFW.EQ.1 ) THEN
C--   Apply uniform correction to Ocean FW Forcing (+ Atm-Flux, for diagnostics)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = jMin,jMax
           DO i = iMin,iMax
             icFrwAtm(i,j,bi,bj) = icFrwAtm(i,j,bi,bj)
     &                           - sumFrW*maskInC(i,j,bi,bj)
             EmPmR(i,j,bi,bj)    = EmPmR(i,j,bi,bj)
     &                           - sumFrW*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( thSIceBalanceAtmFW.EQ.2 ) THEN
C--   Scale correction by local precip and apply it to Ocean FW Forcing
C      (+ Atm-Flux, for diagnostics)
        IF ( sumPrc.GT.0. _d 0 ) THEN
          tmpFac = sumFrW / sumPrc
        ELSE
          tmpFac = 0.
          _BEGIN_MASTER(myThid)
          adjustFrW = 0. _d 0
          _END_MASTER(myThid)
        ENDIF
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = jMin,jMax
           DO i = iMin,iMax
             tmpVar = tmpFac*MAX( prcAtm(i,j,bi,bj), zeroRL )
     &                      *maskInC(i,j,bi,bj)
             icFrwAtm(i,j,bi,bj) = icFrwAtm(i,j,bi,bj) - tmpVar
             EmPmR(i,j,bi,bj)    = EmPmR(i,j,bi,bj)    - tmpVar
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( thSIceBalanceAtmFW.NE.0 ) THEN
        STOP
     &  'ABNORMAL END: THSICE_BALANCE_FRW: invalid thSIceBalanceAtmFW'
      ENDIF

#endif /* ALLOW_BALANCE_FLUXES */

      RETURN
      END
