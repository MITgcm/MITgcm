C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_adv_vu.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_ADV_VU(
     I        bi,bj,k,
     I        vTrans, uFld,
     O        AdvectFluxVU,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate meridional advective flux of U using centered second order diff.
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AdvectFluxVU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        AdvectFluxVU(i,j) =
     &  0.25*( vTrans(i,j) + vTrans(i-1,j) )
     &      *( uFld(i,j) + uFld(i,j-1) )
#ifdef OLD_ADV_BCS
     &      *_maskW(i,j,k,bi,bj)
     &      *_maskW(i,j-1,k,bi,bj)
#endif /* OLD_ADV_BCS */
       ENDDO
      ENDDO

      RETURN
      END
