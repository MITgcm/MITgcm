#include "STREAMICE_OPTIONS.h"

      INTEGER FUNCTION ADFIXEDPOINT_NOTREDUCED(CUMUL, REDUCTION)
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION IO\_ERRCOUNT                                     |
C     | o Reads IO error counter.                                |
C     *==========================================================*

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE_FP.h"

C     !INPUT PARAMETERS:
C     == Routine arguments ==
      _RL CUMUL
      _RL REDUCTION
#ifdef ALLOW_TAPENADE
      _RL CUMUL_GLOB
      _RL CUMUL_TILE(1:nSx, 1:nSy)
      INTEGER iterate
      INTEGER growth
      INTEGER bi, bj, jtlo, jthi, itlo, ithi
CEOP

      IF (nTx.gt.1 .or. nTy.gt.1) THEN
       STOP 'ADFIXEDPOINT_NOTREDUCED: multithread not available'
      ENDIF

      DO bi=1,nSx
        DO bj=1,nSy
          CUMUL_TILE(bi,bj)=0.0
        ENDDO
      ENDDO
      CUMUL_TILE(1,1) = CUMUL

      CALL GLOBAL_SUM_TILE_RL
     &   ( CUMUL_TILE, CUMUL_GLOB, 1 )

      CUMUL = CUMUL_GLOB

      IF (cumul .LT. 0.0D0) THEN
C         Begin 1st iteration of a new adjoint FP loop
          IF (fpDepth .GE. 5) THEN
              adFixedPoint_notReduced = 0
              RETURN
          ENDIF
          fpDepth = fpDepth + 1
          refCumuls(fpDepth) = -1.0D0
          prevCumuls(fpDepth) = -1.0D0
          adjIters(fpDepth) = 1
          adFixedPoint_notReduced = 1
#ifdef ALLOW_PETSC
          if (STREAMICE_OAD_petsc_reuse) then
            streamice_need2createmat=.true.
            streamice_need2destroymat=.false.
          endif
#endif
          RETURN
      ELSE
C         2nd or later iteration
          IF (refCumuls(fpDepth) .LT. 0.0D0) THEN
C             Set reference value
              refCumuls(fpDepth) = cumul
              prevCumuls(fpDepth) = cumul
              IF (cumul .GT. 1.0D-10) THEN
                  iterate = 1
              ELSE
                  iterate = 0
              ENDIF
              growth = 0
          ELSE
C             Compare with reference value
              IF (cumul .GT. streamice_nonlin_tol_adjoint_rl *
     &                       refCumuls(fpDepth)) THEN
                  iterate = 1
              ELSE
                  iterate = 0
              ENDIF
              IF (adjIters(fpDepth) .GT. 5 .AND.
     &             cumul .GT. prevCumuls(fpDepth)) THEN
                  growth = 1
              ELSE
                  growth = 0
              ENDIF
              prevCumuls(fpDepth) = cumul
          ENDIF

          IF (iterate .EQ. 1 .AND. growth .EQ. 0) THEN
              adjIters(fpDepth) = adjIters(fpDepth) + 1
              WRITE(*,*) adjIters(fpDepth),
     &                 ' adjoint iterations (reduced ',
     &                 refCumuls(fpDepth), ' -> ', cumul, ')'
              adFixedPoint_notReduced = 1
#ifdef ALLOW_PETSC
              if (STREAMICE_OAD_petsc_reuse) then
                streamice_need2createmat=.false.
                streamice_need2destroymat=.false.
              endif
#endif
              RETURN
          ELSE
              IF (growth .EQ. 1) THEN
                  WRITE(*,*) adjIters(fpDepth),
     &                 ' adjoint iterations (reduced ',
     &                 refCumuls(fpDepth), ' -> ', cumul,
     &                 ', TERMINATED)'
#ifdef ALLOW_PETSC
                  if (STREAMICE_OAD_petsc_reuse) then
                    streamice_need2createmat=.false.
                    streamice_need2destroymat=.true.
                    !CALL streamice_petscmatdestroy()
                  endif
#endif
              ELSE
                  WRITE(*,*) adjIters(fpDepth),
     &                 ' adjoint iterations (reduced ',
     &                 refCumuls(fpDepth), ' -> ', cumul,
     &                 '), CONVERGED'
#ifdef ALLOW_PETSC
                  if (STREAMICE_OAD_petsc_reuse) then
                    streamice_need2createmat=.false.
                    streamice_need2destroymat=.true.
                    !CALL streamice_petscmatdestroy()
                  endif
#endif
              ENDIF

              IF (fpDepth .LT. 0) THEN
                  adFixedPoint_notReduced = 0
#ifdef ALLOW_PETSC
                  if (STREAMICE_OAD_petsc_reuse) then
                    streamice_need2createmat=.false.
                    streamice_need2destroymat=.true.
                    !CALL streamice_petscmatdestroy()
                  endif
#endif
                  RETURN
              ENDIF
              fpDepth = fpDepth - 1
              adFixedPoint_notReduced = 0
              RETURN
          ENDIF
      ENDIF
#endif /* ALLOW_TAPENADE */
      END
