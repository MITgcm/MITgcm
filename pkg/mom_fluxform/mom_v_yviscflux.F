C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_yviscflux.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_YVISCFLUX(
     I        bi,bj,k,
     I        vFld, del2v,
     O        yViscFluxV,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate viscous flux in Y direction for V eqn
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL yViscFluxV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        yViscFluxV(i,j) =
     & _dxF(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
     &     *(
     &       -viscAh*(vFld(i,j+1)-vFld(i,j))
#ifdef ISOTROPIC_COS_SCALING
     &       *cosFacU(J,bi,bj)
#endif
     &       +viscA4*(del2v(i,j+1)       -del2v(i,j)       )
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &       *sqCosFacU(J,bi,bj)
#else
     &       *cosFacU(J,bi,bj)
#endif
#endif
     &      )*_recip_dyF(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
