C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_adv_uv.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_ADV_UV(
     I        bi,bj,k,
     I        uTrans, vFld,
     O        AdvectFluxUV,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate zonal advective flux of V using centered second order diff.
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AdvectFluxUV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        AdvectFluxUV(i,j) =
     &  0.25*( uTrans(i,j) + uTrans(i,j-1) )
     &      *(   vFld(i,j) +   vFld(i-1,j) )
#ifdef OLD_ADV_BCS
     &      *_maskS(i,j,k,bi,bj)
     &      *_maskS(i-1,j,k,bi,bj)
#endif /* OLD_ADV_BCS */
       ENDDO
      ENDDO

      RETURN
      END
