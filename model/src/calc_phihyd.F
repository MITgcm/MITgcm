C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_phihyd.F,v 1.1 1998/08/20 16:27:13 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CALC_PHI_HYD( bi, bj, iMin, iMax, jMin, jMax, K,
     I    buoyKM1, buoyKP1k, phiHyd, myThid)
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
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz)
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

C--   Contribution to phiHyd(:,:,K-1) from buoy(:,:,K-1)
C     (This is now the actual hydrostatic pressure|height at the T/S points)
      DO j=jMin,jMax
       DO i=iMin,iMax
        phiHyd(i,j,K)=phiHyd(i,j,Km1)-Gravity*halfLayer
     &  *0.5 _d 0*(drF(Km1)+drF(K))
     &  *0.5 _d 0*( buoyKM1(i,j)+buoyKP1(i,j) )
       ENDDO
      ENDDO

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
