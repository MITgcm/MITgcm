C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_u_grad_ke.F,v 1.3 2004/07/20 17:46:38 adcroft Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_U_GRAD_KE( 
     I        bi,bj,K,
     I        KE,
     O        dKEdx,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_U_GRAD_KE                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dKEdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

       DO J=1-Oly,sNy+Oly
        DO I=2-Olx,sNx+Olx
         dKEdx(I,J)=-_recip_dxC(i,j,bi,bj)*(KE(i,j)-KE(i-1,j))
     &       *_maskW(i,j,k,BI,BJ)
        ENDDO
       ENDDO

      RETURN
      END
