C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_v_yviscflux.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_V_YVISCFLUX(
     I        bi,bj,k,
     I        hDiv, del2v,
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
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
     &     *(-viscAh*hDiv(i,j)
#ifdef ISOTROPIC_COS_SCALING
     &       *cosFacU(J,bi,bj)
#ifdef COSINEMETH_III
     &       +viscA4*(del2v(i,j+1)-del2v(i,j))*sqCosFacU(J,bi,bj)
#else
     &       +viscA4*(del2v(i,j+1)-del2v(i,j))*cosFacU(J,bi,bj)
#endif
     &      *_recip_dyF(i,j,bi,bj)
#endif
     &      )
       ENDDO
      ENDDO

      RETURN
      END
