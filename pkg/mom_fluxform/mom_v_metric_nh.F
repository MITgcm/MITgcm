C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_v_metric_nh.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_METRIC_NH(
     I        bi,bj,k,
     I        vFld,wFld,
     O        vMetricTerms,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate spherical metric terms in V equation
C       - is obviously not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1
      _RL wVelBottomOverride

      Kp1=min(k+1,Nr)
      wVelBottomOverride=1.
      IF (k.EQ.Nr) wVelBottomOverride=0.

      DO j=1-Olx+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
         vMetricTerms(i,j) = -vFld(i,j)*recip_RSphere
     &    *0.25*(wVelBottomOverride*
     &           (wFld(i,j,kp1,bi,bj)+wFld(i,j-1,kp1,bi,bj))
     &           +wFld(i,j, k ,bi,bj)+wFld(i,j-1, k ,bi,bj)
     &          )*rkFac*recip_horiVertRatio
       ENDDO
      ENDDO

      RETURN
      END
