C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_v_rviscflux.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_RVISCFLUX(
     I        bi,bj,k,
     I        vFld, KappaRV,
     O        rViscFluxV,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate viscous flux in R direction for V eqn
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
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rViscFluxV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1

      Kp1=min(K+1,Nr)

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        rViscFluxV(i,j) =
     &   -KappaRV(i,j,kp1)
     &   *rAs(i,j,bi,bj)
     &   *( vFld(i,j,k,bi,bj)-vFld(i,j,kp1,bi,bj)
     &    )*rkFac*recip_drC(kp1)
     &    *_maskS(i,j,kp1,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
