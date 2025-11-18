#include "CPP_EEOPTIONS.h"
#include "TAPENADE_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

      SUBROUTINE GLOBAL_MAX_R8_B(rhsmax, rhsmaxb, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_MAX.h"
      _RL     rhsmax
      _RL     rhsmaxb
      INTEGER myThid

      CALL GLOBAL_ADMAX_R8(rhsmaxb, myThid)
      RETURN
      END

      SUBROUTINE GLOBAL_SUM_TILE_RL_B(phiTile, phiTileb,
     &     sumPhi, sumPhib, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL     phiTile(nSx,nSy)
      _RL     phiTileb(nSx,nSy)
      _RL     sumPhib
      _RL     sumPhi
      INTEGER myThid

      CALL GLOBAL_ADSUM_TILE_RL(phiTileb, sumPhib, myThid)
      END

      SUBROUTINE GLOBAL_SUM_TILE_RL_FWD(phiTile,
     &     sumPhi, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL     phiTile(nSx,nSy)
      _RL     sumPhi
      INTEGER myThid

      CALL PUSHINTEGER4(myThid)
      CALL GLOBAL_SUM_TILE_RL(phiTile, sumPhi, myThid)
      CALL PUSHREAL8(sumPhi)
      END

      SUBROUTINE GLOBAL_SUM_TILE_RL_BWD(phiTile, phiTileb,
     &     sumPhi, sumPhib, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL     phiTile(nSx,nSy)
      _RL     phiTileb(nSx,nSy)
      _RL     sumPhib
      _RL     sumPhi
      INTEGER myThid

      CALL POPREAL8(sumPhi)
      CALL POPINTEGER4(myThid)
      CALL GLOBAL_ADSUM_TILE_RL(phiTileb, sumPhib, myThid)
      END

!     Adjoint of global_sum_r8 for arguments Arg1=(in;out)
      SUBROUTINE GLOBAL_SUM_R8_B(sumPhi, sumPhib, myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
      _RL sumPhi
      _RL sumPhib
      INTEGER myThid

      CALL GLOBAL_ADSUM_R8(sumPhib, myThid)
      END

      SUBROUTINE CG2D_B0(cg2d_b, cg2d_bb, cg2d_x,
     &                cg2d_xb, firstResidual, minResidualSq,
     &                lastResidual,numIters, nIterMin,myThid)
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CG2D.h"
      _RL  cg2d_b(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_bb(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_x(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  cg2d_xb(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy)
      _RL  firstResidual
      _RL  minResidualSq
      _RL  lastResidual
      INTEGER numIters
      INTEGER nIterMin
      INTEGER myThid

! [llh] we assume the downstream cg2d_b is passive, which helps us
!    because it seems the input 2nd arg of cg2d() pollutes its output value:
      cg2d_bb = 0.d0
      CALL CG2D(cg2d_xb, cg2d_bb, firstResidual,
     +        minResidualSq, lastResidual, numIters,
     +        nIterMin, myThid)
! [llh] the upstream cg2d_x is passive:
      cg2d_xb = 0.d0
      END

      SUBROUTINE ADEXCH_3D_RL( adVar, Nr, myThid )
      IMPLICIT NONE
      Real*8 adVar
      INTEGER Nr
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_UV_XY_RS( adU, adV, bool, myThid )
      IMPLICIT NONE
      Real*8 adU
      Real*8 adV
      LOGICAL bool
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_UV_3D_RL( aduVel,advVel, bool, Nr, myThid )
      IMPLICIT NONE
      LOGICAL bool
      Real*8 aduVel
      Real*8 advVel
      INTEGER Nr
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_XY_RS( adVar, myThid )
      IMPLICIT NONE
      Real*8 adVar
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

      SUBROUTINE ADEXCH_XY_RL( adVar, myThid )
      IMPLICIT NONE
      Real*8 adVar
      INTEGER myThid
      WRITE(*,*) "Called not yet defined"
      END

