C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_depth.F,v 1.4 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      subroutine cost_depth( myThid )
C     /==========================================================\
C     | subroutine cost_depth                                    |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     |==========================================================|
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST_DEPTH
C     == Local variables

      INTEGER I, J, K

      character*(max_len_mbuf) msgbuf

C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          IF ( maskC(I,J,1,BI,BJ) .NE. 0. ) THEN
           objf_depth(BI,BJ) = objf_depth(BI,BJ) + R_low(I,J,BI,BJ)
          ENDIF
         END DO
        END DO
       END DO
      END DO
#endif /* ALLOW_COST_DEPTH */

      RETURN
      END
