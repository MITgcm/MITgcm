C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_metric_sphere.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_METRIC_SPHERE(
     I        bi,bj,k,
     I        uFld,
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
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

      DO j=1-Olx+1,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
         vMetricTerms(i,j) = -recip_RSphere
     &    *0.25*( uFld(i,j  )+uFld(i+1,j  )
     &           +uFld(i,j-1)+uFld(i+1,j-1)
     &          )
     &    *0.25*( uFld(i,j  )+uFld(i+1,j  )
     &           +uFld(i,j-1)+uFld(i+1,j-1)
     &          )
     &    *_tanPhiAtV(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
