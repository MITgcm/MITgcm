C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_v_grad_ke.F,v 1.3 2004/07/20 17:46:38 adcroft Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_V_GRAD_KE( 
     I        bi,bj,K,
     I        KE,
     O        dKEdy,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_V_GRAD_KE                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dKEdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

       DO J=2-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx
         dKEdy(I,J)=-_recip_dyC(i,j,bi,bj)*(KE(i,j)-KE(i,j-1))
     &       *_maskS(i,j,k,BI,BJ)
        ENDDO
       ENDDO

      RETURN
      END
