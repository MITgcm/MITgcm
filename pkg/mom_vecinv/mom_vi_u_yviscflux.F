C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_u_yviscflux.F,v 1.3 2001/08/16 20:18:08 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_U_YVISCFLUX(
     I        bi,bj,k,
     I        vort3,del2u,hFacZ,
     O        yViscFluxU,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate viscous flux in Y direction for U eqn
C       - is not vector invariant ...
C

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL yViscFluxU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        yViscFluxU(i,j) =
     &     _dxV(i,j,bi,bj)*drF(k)*hFacZ(i,j)
     &     *(-viscAh*(-vort3(i,j))
#ifdef ISOTROPIC_COS_SCALING
     &       *cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
     &       +viscA4*(del2u(i,j)-del2u(i,j-1))*sqcosFacV(J,bi,bj)
#else
     &       +viscA4*(del2u(i,j)-del2u(i,j-1))*cosFacV(J,bi,bj)
#endif
     &      *recip_dyu(i,j,bi,bj)
#endif
     &      )
       ENDDO
      ENDDO

      RETURN
      END
