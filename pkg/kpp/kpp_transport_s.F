C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_transport_s.F,v 1.7 2001/05/29 14:01:38 adcroft Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

      subroutine KPP_TRANSPORT_S (
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     KappaRS,
     U     df )
C     /==========================================================\
C     | o SUBROUTINE KPP_TRANSPORT_S                             |
C     |   Add non local KPP transport term (ghat) to diffusive   |
C     |   salinity flux.                                         |
C     |==========================================================|
C     | The nonlocal transport term is nonzero only for scalars  |
C     | in unstable (convective) forcing conditions.             |
C     | Note: KappaRS(i,j,k) is defined at the top of grid cell  |
C     |       k while KPPghat(i,j,k,bi,bj) is defined at the     |
C     |       bottom of grid cell k.                             |
C     |       For K=1, KappaRS(i,j,k) = 0 which insures no flux  |
C     |       through the surface.                               |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k,km1   results will be set.
C     KappaRS          - Total diffusion in vertical for S.
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k,km1
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL df     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#if defined(ALLOW_KPP) && defined(KPP_GHAT)

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J

      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j) - _rA(i,j,bi,bj) * 
     &         KappaRS(i,j,k) * KPPghat(i,j,km1,bi,bj) * delZ(1) *
     &         SurfaceTendencyS(i,j,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_KPP and KPP_GHAT */

      RETURN
      END
