C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_metric_sphere.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_METRIC_SPHERE(
     I        bi,bj,k,
     I        uFld, vFld,
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
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

      DO j=1-Olx,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx
         uMetricTerms(i,j) = 
     &    uFld(i,j)*recip_RSphere
     &    *0.25*( vFld(i,j  )+vFld(i-1,j  )
     &           +vFld(i,j+1)+vFld(i-1,j+1)
     &          )*_tanPhiAtU(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
