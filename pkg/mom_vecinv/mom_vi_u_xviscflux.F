C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_u_xviscflux.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_U_XVISCFLUX(
     I        bi,bj,k,
     I        hDiv,del2u,
     O        xViscFluxU,
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
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xViscFluxU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        xViscFluxU(i,j) =
     &    _dyF(i,j,bi,bj)*drF(k)*_hFacC(i,j,k,bi,bj)
     &     *(-viscAh*hDiv(i,j)*cosFacU(J,bi,bj)
#ifdef COSINEMETH_III
     &       +viscA4*(del2u(i+1,j)-del2u(i,j))*sqcosFacU(J,bi,bj)
#else
     &       +viscA4*(del2u(i+1,j)-del2u(i,j))*cosFacU(J,bi,bj)
#endif
     &      *recip_dxf(i,j,bi,bj) )
       ENDDO
      ENDDO

      RETURN
      END
