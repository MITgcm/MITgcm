C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_u_metric_nh.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_METRIC_NH(
     I        bi,bj,k,
     I        uFld, wFld,
     O        uMetricTerms,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate viscous flux in X direction for U eqn
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1
      _RL wVelBottomOverride

      Kp1=min(k+1,Nr)
      wVelBottomOverride=1.
      IF (k.EQ.Nr) wVelBottomOverride=0.

      DO j=1-Olx,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
         uMetricTerms(i,j) = 
     &    -1.* uFld(i,j)*recip_RSphere
     &    *0.25*(wVelBottomOverride*
     &            (wFld(i-1,j,kp1,bi,bj)+wFld(i,j,kp1,bi,bj))
     &            +wFld(i-1,j, k ,bi,bj)+wFld(i,j, k ,bi,bj)
     &          )*rkFac*recip_horiVertRatio
       ENDDO
      ENDDO

      RETURN
      END
