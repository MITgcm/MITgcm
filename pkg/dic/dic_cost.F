C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_cost.F,v 1.12 2009/10/16 15:48:33 heimbach Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE DIC_COST (
     I           myTime,myIter,myThid)

C     /==========================================================\
C     | SUBROUTINE DIC_COST                              |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#ifdef DIC_BIOTIC
#include "DIC_DIAGS.h"
#include "DIC_COST.h"
#endif

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_COST

C     == Local variables ==
      INTEGER i, j, bi, bj, k, it
      _RL tmpcost
c
c
c calculate costfunction

      tmpcost = 0. _d 0
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO i=1,sNx
          DO j=1,sNy
              tmpcost = tmpcost+FluxCO2(i,j,bi,bj)*rA(i,j,bi,bj)*
     &                        maskC(i,j,1,bi,bj)*dTtracerLev(1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_RL(tmpcost,myThid)

       totcost = totcost + tmpcost

       print *, 'QQ myiter, totcost = ', myiter, totcost

#endif

      RETURN
      END
cswd -- end added subroutine --
