C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_transport_ptr.F,v 1.3 2003/09/24 04:52:38 dimitri Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
#include "PTRACERS_OPTIONS.h"
#endif /* ALLOW_PTRACERS */
#include "KPP_OPTIONS.h"

      subroutine KPP_TRANSPORT_PTR (
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     I     tracerIdentity,KappaRtr,
     U     df )
C     /==========================================================\
C     | o SUBROUTINE KPP_TRANSPORT_PTR                           |
C     |   Add non local KPP transport term (ghat) to diffusive   |
C     |   passive tracer flux.                                   |
C     |==========================================================|
C     | The nonlocal transport term is nonzero only for scalars  |
C     | in unstable (convective) forcing conditions.             |
C     | Note: KappaRtr(i,j,k) is defined at the top of grid cell |
C     |       k while KPPghat(i,j,k,bi,bj) is defined at the     |
C     |       bottom of grid cell k.                             |
C     |       For K=1, KappaRtr(i,j,k) = 0 which insures no flux |
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
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#endif

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k,km1   results will be set.
C     tracerIdentity   - Identifier for the tracer
C     KappaRtr         - Total diffusion in vertical for passive tracer.
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k,km1,tracerIdentity
      _RL KappaRtr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL df      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#if defined(ALLOW_KPP) && defined(KPP_GHAT) && defined(ALLOW_PTRACERS)

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J

      DO j=jMin,jMax
       DO i=iMin,iMax
         df(i,j) = df(i,j) - _rA(i,j,bi,bj) * 
     &          KappaRtr(i,j,k) * KPPghat(i,j,km1,bi,bj) * drF(1) *
     &          surfaceTendencyPtr(i,j,bi,bj,tracerIdentity)
       ENDDO
      ENDDO

#endif /* ALLOW_KPP and KPP_GHAT and PTRACERS */

      RETURN
      END
