C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_xviscflux.F,v 1.2 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_V_XVISCFLUX(
     I        bi,bj,k,
     I        vFld, del2v, hFacZ,
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
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
     &     *(
     &       -viscAh*(vFld(i,j)-vFld(i-1,j))
     &       *cosFacV(J,bi,bj)
     &       +viscA4*(del2v(i,j)       -del2v(i-1,j)       )
#ifdef COSINEMETH_III
     &      *sqcosFacV(J,bi,bj)
#else
     &      *cosFacV(J,bi,bj)
#endif
     &      )*_recip_dxV(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
