C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_u_sidedrag.F,v 1.3 2004/02/08 01:08:30 dimitri Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_U_SIDEDRAG(
     I        bi,bj,k,
     I        uFld, del2u, hFacZ,
     O        uDragTerms,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate side drag terms for U eqn
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
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RS hFacZClosedS,hFacZClosedN

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
        uDragTerms(i,j) =
     &   -_recip_hFacW(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAw(i,j,bi,bj)
     &   *( hFacZClosedS*_dxV(i, j ,bi,bj)
     &      *_recip_dyU(i, j ,bi,bj)
     &     +hFacZClosedN*_dxV(i,j+1,bi,bj)
     &      *_recip_dyU(i,j+1,bi,bj) )
     &   *drF(k)*2.*(
#ifdef ISOTROPIC_COS_SCALING
     &                viscAh*uFld(i,j)*cosFacU(J,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2u(i,j)*sqcosFacU(J,bi,bj)
#else
     &               -viscA4*del2u(i,j)*cosFacU(J,bi,bj)
#endif
#else
     &     min(viscAh+viscAhGrid* rA(i,j,bi,bj)    /deltaTmom,viscAhMax)
     &                *uFld(i,j)
     &    -min(viscA4+viscA4Grid*(rA(i,j,bi,bj)**2)/deltaTmom,viscA4Max)
     &                *del2u(i,j)
#endif
     &              )
       ENDDO
      ENDDO

      RETURN
      END
