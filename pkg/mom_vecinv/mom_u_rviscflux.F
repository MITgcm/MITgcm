C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_u_rviscflux.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_RVISCFLUX(
     I        bi,bj,k,
     I        uFld, KappaRU,
     O        rViscFluxU,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate viscous flux in R direction for U eqn
C     This only calculates interior fluxes and assumes zero
C     flux at solid boundaries.  "no-slip" fluxes are added
C     as a drag (mom_u_bottomdrag.F).
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rViscFluxU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1

      Kp1=min(K+1,Nr)

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        rViscFluxU(i,j) =
     &   -KappaRU(i,j,kp1)
     &   *rAw(i,j,bi,bj)
     &   *( uFld(i,j,k,bi,bj)-uFld(i,j,kp1,bi,bj)
     &    )*rkFac*recip_drC(kp1)
     &    *_maskW(i,j,kp1,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
