C $Header: /u/gcmpack/MITgcm/model/src/calc_wsurf_tr.F,v 1.1 2007/01/05 01:35:00 dfer Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_WSURF_TR
C     !INTERFACE:
      SUBROUTINE CALC_WSURF_TR(thetaFld, saltFld, wVelFld,
     I                         myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_WSURF_TR
C     | o Compute a correction for the source/sink of tracer
C     |   due to the linear free surface.
C     | o The source/sink results from W*Tr not summing to
C     |   zero at the free surface.
C     | o Here, we compute an area-weighted mean correction 
C     |   to be applied in external_forcing.F
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime   :: Current time in simulation
C     myIter   :: Current iteration number in simulation
C     myThid   :: Thread number for this instance of the routine.
C     thetaFld :: Potential Temperature field
C     saltFld  :: Salinity field
C     wvelFld  :: vertical velocity field
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      _RL thetaFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL saltFld (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL wVelFld (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
      INTEGER i,j,bi,bj,ks
      _RL wT_Mean, wS_Mean
      _RL wT_Tile, wS_Tile
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      TsurfCor=0.
      SsurfCor=0.

      wT_Mean = 0.
      wS_Mean = 0.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
         wT_Tile = 0.
         wS_Tile = 0.
         DO j=1,sNy
          DO i=1,sNx
             ks = ksurfC(i,j,bi,bj)
             IF (ks.LE.Nr) THEN
                wT_Tile = wT_Tile 
     &           + rA(i,j,bi,bj)*wVelFld(i,j,ks,bi,bj)
     &                          *thetaFld(i,j,ks,bi,bj)
                wS_Tile = wS_Tile
     &           + rA(i,j,bi,bj)*wVelFld(i,j,ks,bi,bj)
     &                          *saltFld(i,j,ks,bi,bj)
             ENDIF
          ENDDO
         ENDDO
         wT_Mean = wT_Mean   + wT_Tile
         wS_Mean = wS_Mean   + wS_Tile
C-     end bi,bj loop.
       ENDDO
      ENDDO

C-- Global diagnostic :
      _GLOBAL_SUM_R8(wT_Mean,myThid)
      _GLOBAL_SUM_R8(wS_Mean,myThid)
      IF (globalArea.GT.0.) THEN
        _BEGIN_MASTER( myThid )
        TsurfCor = wT_Mean / globalArea
        SsurfCor = wS_Mean / globalArea
        _END_MASTER( myThid )
      ENDIF
      _BARRIER

C-----

      RETURN
      END
