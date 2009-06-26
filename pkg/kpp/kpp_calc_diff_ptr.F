C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_calc_diff_ptr.F,v 1.1 2009/06/26 23:10:09 jahn Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_CALC_DIFF_PTR(
     I        bi,bj,iMin,iMax,jMin,jMax,kArg,kSize,
     O        KappaRx,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE KPP_CALC_DIFF_PTR                             |
C     | o Return contribution to net diffusivity from KPP mixing |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP.h"
#endif
#ifdef ALLOW_KPP
#include "KPP.h"
#endif

C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     iMin,iMax :: Range of points for which calculation is done
C     jMin,jMax :: Range of points for which calculation is done
C     kArg      :: = 0 -> do the k-loop here and treat all levels
C                  > 0 -> k-loop is done outside and treat only level k=kArg
C     kSize     :: 3rd Dimension of the vertical diffusivity array KappaRS
C     KappaRx   :: vertical diffusivity array
C     myThid    :: Instance number for this innvocation of KPP_CALC_DIFF_S
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,kArg,kSize
      _RL KappaRx(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize)
      INTEGER myThid

#ifdef ALLOW_KPP

C     == Local variables ==
C     i,j,k     :: Loop counters
      INTEGER i,j,k

C--   Set vertical diffusivity contribution from KPP
      IF ( kArg .EQ. 0 ) THEN
C-    do all levels :
       DO k=1,MIN(Nr,kSize)
        DO j=jMin,jMax
         DO i=iMin,iMax
#ifdef ALLOW_LONGSTEP
          KappaRx(i,j,k) = LS_KPPdiffKzS(i,j,k,bi,bj)
#else
          KappaRx(i,j,k) = KPPdiffKzS(i,j,k,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDDO
      ELSE
C-    do level k=kArg only :
       k = MIN(kArg,kSize)
       DO j=jMin,jMax
        DO i=iMin,iMax
#ifdef ALLOW_LONGSTEP
         KappaRx(i,j,k) = LS_KPPdiffKzS(i,j,kArg,bi,bj)
#else
         KappaRx(i,j,k) = KPPdiffKzS(i,j,kArg,bi,bj)
#endif
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_KPP */

      RETURN
      END
