C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_v_sidedrag.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_SIDEDRAG(
     I        bi,bj,k,
     I        vFld, del2v, hFacZ,
     O        vDragTerms,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate side drag terms for V eqn
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
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RS hFacZClosedE,hFacZClosedW

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        vDragTerms(i,j) =
     &   -_recip_hFacS(i,j,k,bi,bj)
     &   *recip_drF(k)*recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*_dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*_dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj) )
     &   *drF(k)*2.*(
     &                viscAh*vFld(i,j)*cosFacV(J,bi,bj)
     &               -viscA4*del2v(i,j)*cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
     &               -viscA4*del2v(i,j)*sqcosFacV(J,bi,bj)
#else
     &               -viscA4*del2v(i,j)*cosFacV(J,bi,bj)
#endif
     &              )
       ENDDO
      ENDDO

      RETURN
      END
