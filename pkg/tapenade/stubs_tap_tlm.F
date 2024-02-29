#include "CPP_EEOPTIONS.h"

      SUBROUTINE GLOBAL_SUM_TILE_RL_D(phiTile, phiTiled,
     &     sumPhi, sumPhid, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL     phiTile(nSx,nSy)
      _RL     phiTiled(nSx,nSy)
      _RL     sumPhid
      _RL     sumPhi
      INTEGER myThid
      CALL GLOBAL_SUM_TILE_RL(phiTiled, sumPhid, myThid)
      CALL GLOBAL_SUM_TILE_RL(phiTile, sumPhi, myThid)
      END

      SUBROUTINE GLOBAL_SUM_R8_D(sumPhi, sumPhid, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL sumPhi
      _RL sumPhid
      INTEGER myThid
      CALL GLOBAL_SUM_R8(sumPhid, myThid)
      CALL GLOBAL_SUM_R8(sumPhi, myThid)
      END

      SUBROUTINE CG2D_D(cg2d_b, cg2d_bd, cg2d_x,
     &                cg2d_xd, firstResidual, minResidualSq,
     &                lastResidual,numIters, nIterMin,myThid )
      IMPLICIT NONE
#include "SIZE.h"
      _RL  cg2d_b(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_bd(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_xd(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  firstResidual
      _RL  minResidualSq
      _RL  lastResidual
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid
      INTEGER copyIt1, copyIt2

      copyIt1 = numIters
      copyIt2 = nIterMin
! [llh] it seems the input 2nd arg of cg2d() pollutes its output value:
      cg2d_xd = 0.d0
      CALL CG2D(cg2d_bd, cg2d_xd, firstResidual,
     &        minResidualSq, lastResidual, numIters,
     &        nIterMin, myThid)
! [llh] we assume the downstream cg2d_b is passive:
      cg2d_bd = 0.d0
      CALL CG2D(cg2d_b, cg2d_x, firstResidual,
     &        minResidualSq, lastResidual, copyIt1,
     &        copyIt2, myThid)
      END

      SUBROUTINE GLOBAL_MAX_R8_D(rhsmax, rhsmaxd, myThid)
      IMPLICIT NONE
      _RL     rhsmax
      _RL     rhsmaxd
      INTEGER myThid
      print *, "NOT IMPLEMENTED GLOBAL_MAX_R8_D yet."
      RETURN
      END

