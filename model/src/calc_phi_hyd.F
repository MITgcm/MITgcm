C $Header: /u/gcmpack/MITgcm/model/src/calc_phi_hyd.F,v 1.3 1998/09/05 17:52:13 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CALC_PHI_HYD( bi, bj, iMin, iMax, jMin, jMax, K,
     I    buoyKM1, buoyKP1, phiHyd, myThid)
C     /==========================================================\
C     | SUBROUTINE CALC_PHI_HYD                                  |
C     | o Integrate the hydrostatic relation to find phiHyd.     |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL buoyKM1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoyKP1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      integer myThid
C     == Local variables ==
      INTEGER i,j,Km1
      _RL halfLayer

      if (K.eq.1) then
       Km1=1
       halfLayer=0.5 _d 0
      else
       Km1=K-1
       halfLayer=1.0 _d 0
      endif

C--   Contribution to phiHyd(:,:,K) from buoy(:,:,K-1) + buoy(:,:,K)
C     (This is now the actual hydrostatic pressure|height at the T/S points)
      DO j=jMin,jMax
       DO i=iMin,iMax
        phiHyd(i,j,K)=phiHyd(i,j,Km1)-rhoConst*halfLayer
     &  *0.5 _d 0*(drF(Km1)+drF(K))
     &  *0.5 _d 0*( buoyKM1(i,j)+buoyKP1(i,j) )
     &  *rkFac
       ENDDO
      ENDDO

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
