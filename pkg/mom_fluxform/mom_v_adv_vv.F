C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_adv_vv.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_ADV_VV(
     I        bi,bj,k,
     I        vTrans, vFld,
     O        AdvectFluxVV,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate meridional advective flux of V using centered second order diff.
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AdvectFluxVV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        AdvectFluxVV(i,j) =
     &  0.25*( vTrans(i,j) + vTrans(i,j+1) )
     &      *(   vFld(i,j) +   vFld(i,j+1) )
       ENDDO
      ENDDO

      RETURN
      END
