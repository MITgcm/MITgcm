C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_transport_t.F,v 1.5 2000/11/29 22:29:23 adcroft Exp $

#include "KPP_OPTIONS.h"

      subroutine KPP_TRANSPORT_T (
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     maskC,KappaRT,
     U     df )
C     /==========================================================\
C     | o SUBROUTINE KPP_TRANSPORT_T                             |
C     |   Add non local KPP transport term (ghat) to diffusive   |
C     |   temperature flux.                                      |
C     |==========================================================|
C     | The nonlocal transport term is nonzero only for scalars  |
C     | in unstable (convective) forcing conditions.             |
C     | Note: [SurfaceTendencyT + Qsw * (1-KPPfrac)]             |
C     |       is the total heat flux penetrating the mixed layer |
C     |       from the surface in deg C / s referenced to the    |
C     |       top layer.                                         |
C     | Note: KappaRT(i,j,k) is defined at the top of grid cell  |
C     |       k while KPPghat(i,j,k,bi,bj) is defined at the     |
C     |       bottom of grid cell k.                             |
C     |       For K=1, KappaRT(i,j,k) = 0 which insures no flux  |
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
C     maskC            - Land mask for salt cells.
C     KappaRT          - Total diffusion in vertical for T.
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k,km1
      _RS maskC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL df     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#if defined(ALLOW_KPP) && defined(KPP_GHAT)

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J

      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j) - _rA(i,j,bi,bj) * maskC(i,j) *
     &         KappaRT(i,j,k) * KPPghat(i,j,km1,bi,bj) * delZ(1) *
     &         ( SurfaceTendencyT(i,j,bi,bj) -
     &           Qsw(i,j,bi,bj)
     &          *recip_Cp*recip_rhoNil*recip_dRf(1)
     &          * (1.-KPPfrac(i,j,bi,bj))
     &         )
       ENDDO
      ENDDO

#endif /* ALLOW_KPP and KPP_GHAT */

      RETURN
      END
