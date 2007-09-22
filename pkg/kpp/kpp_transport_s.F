C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_transport_s.F,v 1.13 2007/09/22 17:55:32 dimitri Exp $
C $Name:  $

#include "KPP_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

      subroutine KPP_TRANSPORT_S (
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     O     df,
     I     myTime, myIter, myThid )
C     /==========================================================\
C     | o SUBROUTINE KPP_TRANSPORT_S                             |
C     |   Add non local KPP transport term (ghat) to diffusive   |
C     |   salinity flux.                                         |
C     |==========================================================|
C     | The nonlocal transport term is nonzero only for scalars  |
C     | in unstable (convective) forcing conditions.             |
C     | Note: KPPdiffKzS(-,k) is defined at the top of grid cell |
C     |       k while KPPghat(i,j,k,bi,bj) is defined at the     |
C     |       bottom of grid cell k.                             |
C     |       For K=1, KPPdiffKzS(-,k) = 0 which insures no flux |
C     |       through the surface.                               |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "KPP.h"
#include "KPP_PARAMS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k,km1   results will be set.
C     df               - Diffusive flux component work array.
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: My Thread Id. number
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k,km1
      _RL df     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if defined(ALLOW_KPP) && defined(KPP_GHAT)

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J
      _RL tmpFac

#ifdef ALLOW_GMREDI

      IF ( useGMRedi .AND. KPP_ghatUseTotalDiffus ) THEN
        tmpFac = 1. _d 0
      ELSE
        tmpFac = 0. _d 0
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = - _rA(i,j,bi,bj)
     &        *( KPPdiffKzS(i,j,k,bi,bj) + tmpFac*Kwz(i,j,k,bi,bj) )
     &        *KPPghat(i,j,km1,bi,bj)
     &        *( surfaceForcingS(i,j,bi,bj)
#ifdef ALLOW_SALT_PLUME
     &          + saltPlumeFlux(i,j,bi,bj)*recip_rhoConst
     &          *(1.-KPPplumefrac(i,j,bi,bj))
#endif /* ALLOW_SALT_PLUME */
     &         )
       ENDDO
      ENDDO

#else /* ALLOW_GMREDI */

      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = - _rA(i,j,bi,bj)
     &        *KPPdiffKzS(i,j,k,bi,bj)
     &        *KPPghat(i,j,km1,bi,bj)
     &        *( surfaceForcingS(i,j,bi,bj)
#ifdef ALLOW_SALT_PLUME
     &          + saltPlumeFlux(i,j,bi,bj)*recip_rhoConst
     &          *(1.-KPPplumefrac(i,j,bi,bj))
#endif /* ALLOW_SALT_PLUME */
     &         )
       ENDDO
      ENDDO

#endif /* ALLOW_GMREDI */


#endif /* ALLOW_KPP and KPP_GHAT */

      RETURN
      END
