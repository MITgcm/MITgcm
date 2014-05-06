C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_transport_t.F,v 1.20 2014/05/06 15:50:14 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI_OPTIONS.h"
#endif

      SUBROUTINE KPP_TRANSPORT_T (
     I     iMin,iMax,jMin,jMax,bi,bj,k,km1,
     O     df,
     I     myTime, myIter, myThid )
C     *==========================================================*
C     | o SUBROUTINE KPP_TRANSPORT_T
C     |   Add non local KPP transport term (ghat) to diffusive
C     |   temperature flux.
C     *==========================================================*
C     | The nonlocal transport term is nonzero only for scalars
C     | in unstable (convective) forcing conditions.
C     | Note: [surfaceForcingT + Qsw * (1-KPPfrac)]
C     |       is the total heat flux penetrating the mixed layer
C     |       from the surface in deg C / s referenced to the
C     |       top layer.
C     | Note: KPPdiffKzT(-,k) is defined at the top of grid cell
C     |       k while KPPghat(i,j,k,bi,bj) is defined at the
C     |       bottom of grid cell k.
C     |       For K=1, KPPdiffKzT(-,k) = 0 which insures no flux
C     |       through the surface.
C     *==========================================================*
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
C     iMin,iMax,jMin,  :: Range of points for which calculation
C     jMax,bi,bj,k,km1    results will be set.
C     df               :: Diffusive flux component work array.
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
C     i, j :: Loop counters
      INTEGER i, j
      _RL recip_Cp
#ifdef ALLOW_GMREDI
      _RL tmpFac
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      recip_Cp = 1. _d 0 / HeatCapacity_Cp

#ifdef ALLOW_GMREDI

      IF ( useGMRedi .AND. KPP_ghatUseTotalDiffus ) THEN
        tmpFac = 1. _d 0
      ELSE
        tmpFac = 0. _d 0
      ENDIF
      DO j=jMin,jMax
         DO i=iMin,iMax
            df(i,j) = - _rA(i,j,bi,bj)
     &          *( KPPdiffKzT(i,j,k,bi,bj) + tmpFac*Kwz(i,j,k,bi,bj) )
     &          *KPPghat(i,j,km1,bi,bj)
     &          *( surfaceForcingT(i,j,bi,bj)
     &            +surfaceForcingTice(i,j,bi,bj)
     &            -Qsw(i,j,bi,bj)*recip_Cp*recip_rhoConst
     &            *(1.-KPPfrac(i,j,bi,bj))
     &           )
         ENDDO
      ENDDO

#else /* ALLOW_GMREDI */

      DO j=jMin,jMax
         DO i=iMin,iMax
            df(i,j) = - _rA(i,j,bi,bj)
     &          *KPPdiffKzT(i,j,k,bi,bj)
     &          *KPPghat(i,j,km1,bi,bj)
     &          *( surfaceForcingT(i,j,bi,bj)
     &            +surfaceForcingTice(i,j,bi,bj)
     &            -Qsw(i,j,bi,bj)*recip_Cp*recip_rhoConst
     &            *(1.-KPPfrac(i,j,bi,bj))
     &           )
         ENDDO
      ENDDO

#endif /* ALLOW_GMREDI */

#endif /* ALLOW_KPP and KPP_GHAT */

      RETURN
      END
