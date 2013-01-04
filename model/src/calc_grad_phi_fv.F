C $Header: /u/gcmpack/MITgcm/model/src/calc_grad_phi_fv.F,v 1.1 2013/01/04 21:07:07 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GRAD_PHI_FV
C     !INTERFACE:
      SUBROUTINE CALC_GRAD_PHI_FV(
     I                       k, bi, bj, iMin,iMax, jMin,jMax,
     I                       phiHydF, phiHydU, pKappaF, pKappaU,
     O                       dPhiHydX, dPhiHydY,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_GRAD_PHI_FV
C     | o Calculate the gradient of Hydrostatic pressure anomaly
C     |   using Finite-Volume method from S.-J. Lin (QJRMS, 1997)
C     *==========================================================*
C     | o used with sigma-coords - might be useful also with r*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
c#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj      :: tile index
C     iMin,iMax,jMin,jMax :: Loop counters
C     phiHydF    :: hydrostatic potential anomaly at interface k
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     phiHydU    :: hydrostatic potential anomaly at interface k+1 (Upper k)
C     pKappaF    :: (p/Po)^kappa at interface k
C     pKappaU    :: (p/Po)^kappa at interface k+1 (Upper k)
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hyd. Potential
C     myTime :: Current time
C     myIter :: Current iteration number
C     myThid :: Instance number for this call of the routine.
      INTEGER k, bi,bj, iMin,iMax, jMin,jMax
      _RL phiHydF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiHydU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pKappaF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pKappaU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL dpk_dip, dpk_dim
      _RL dpk_djp, dpk_djm
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Zonal & Meridional gradient of potential anomaly
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        dPhiHydX(i,j)  = 0. _d 0
        dPhiHydY(i,j)  = 0. _d 0
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin+1,iMax
        dpk_dip = pKappaF( i ,j) - pKappaU(i-1,j)
        dpk_dim = pKappaF(i-1,j) - pKappaU( i ,j)
        dPhiHydX(i,j) = ( phi0surf(i,j,bi,bj) - phi0surf(i-1,j,bi,bj) )
     &                + (  dpk_dip*( phiHydU(i,j) - phiHydF(i-1,j) )
     &                   + dpk_dim*( phiHydF(i,j) - phiHydU(i-1,j) )
     &                  )/( dpk_dip + dpk_dim )
        dPhiHydX(i,j) = _recip_dxC(i,j,bi,bj)*dPhiHydX(i,j)
c    &                * recip_deepFacC(k)*recip_rhoFacC(k)
       ENDDO
      ENDDO
      DO j=jMin+1,jMax
       DO i=iMin,iMax
        dpk_djp = pKappaF(i, j ) - pKappaU(i,j-1)
        dpk_djm = pKappaF(i,j-1) - pKappaU(i, j )
        dPhiHydY(i,j) = ( phi0surf(i,j,bi,bj) - phi0surf(i,j-1,bi,bj) )
     &                + (  dpk_djp*( phiHydU(i,j) - phiHydF(i,j-1) )
     &                   + dpk_djm*( phiHydF(i,j) - phiHydU(i,j-1) )
     &                  )/( dpk_djp + dpk_djm )
        dPhiHydY(i,j) = _recip_dyC(i,j,bi,bj)*dPhiHydY(i,j)
c    &                * recip_deepFacC(k)*recip_rhoFacC(k)
       ENDDO
      ENDDO

C--   Apply mask:
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         dPhiHydX(i,j) = dPhiHydX(i,j)*_maskW(i,j,k,bi,bj)
         dPhiHydY(i,j) = dPhiHydY(i,j)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
