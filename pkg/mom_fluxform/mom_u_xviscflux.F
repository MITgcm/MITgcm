C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_xviscflux.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_XVISCFLUX(
     I        bi,bj,k,
     I        uFld, del2u,
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
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
     &     *(
     &       -viscAh*(uFld(i+1,j)-uFld(i,j))
     &       *cosFacU(J,bi,bj)
     &       +viscA4*(del2u(i+1,j)       -del2u(i,j)       )
#ifdef COSINEMETH_III
     &       *sqcosFacU(J,bi,bj)
#else
     &       *cosFacU(J,bi,bj)
#endif
     &      )*_recip_dxF(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
