C $Header: /u/gcmpack/MITgcm/model/src/calc_phi_hyd.F,v 1.8 2000/11/13 16:32:57 heimbach Exp $

#include "CPP_OPTIONS.h"

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
      _RL gamma

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

      if (K.eq.1) then
       Km1=1
       halfLayer=0.5 _d 0
      else
       Km1=K-1
       halfLayer=1.0 _d 0
      endif

C--   Scale factor for hydrostatic relation except for ocean in 
C--   pressure coords.
      gamma = 1. _d 0
C--   Scale factor for hydrostatic relation for ocean in pressure 
C--   coords.
      IF ( buoyancyRelation .EQ. 'OCEANIC' .AND. usingPCoords ) THEN
       gamma = recip_Gravity*recip_rhoConst
      ENDIF

C--   Contribution to phiHyd(:,:,K) from buoy(:,:,K-1) + buoy(:,:,K)
C     (This is now the actual hydrostatic pressure|height at the T/S 
C     points)
      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef ALLOW_AUTODIFF_TAMC
CADJ GENERAL
#endif
        phiHyd(i,j,K)=phiHyd(i,j,Km1)-rhoConst*halfLayer
     &  *0.5 _d 0*( drF(Km1)+drF(K) )*gamma
     &  *0.5 _d 0*( buoyKM1(i,j)+buoyKP1(i,j) )
       ENDDO
      ENDDO

#endif

      return
      end
