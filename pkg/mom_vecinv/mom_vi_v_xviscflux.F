C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_v_xviscflux.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_V_XVISCFLUX(
     I        bi,bj,k,
     I        vort3,del2v,hFacZ,
     O        xViscFluxV,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate viscous flux in X direction for V eqn
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
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xViscFluxV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

C     - Laplacian  and bi-harmonic terms
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        xViscFluxV(i,j) =
     &     _dyU(i,j,bi,bj)*drF(k)*hFacZ(i,j)
     &     *(-viscAh*vort3(i,j)*cosFacV(J,bi,bj)
#ifdef COSINEMETH_III
c    &       +viscA4*(del2v(i,j)-del2v(i-1,j))*sqcosFacV(J,bi,bj)
#else
c    &       +viscA4*(del2v(i,j)-del2v(i-1,j))*cosFacV(J,bi,bj)
#endif
c    &      *_recip_dxV(i,j,bi,bj) )
     &      )
       ENDDO
      ENDDO

      RETURN
      END
