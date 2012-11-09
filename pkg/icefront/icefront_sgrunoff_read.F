C $Header: /u/gcmpack/MITgcm/pkg/icefront/Attic/icefront_sgrunoff_read.F,v 1.6 2012/11/09 22:47:17 jmc Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

      SUBROUTINE SGRUNOFF_READ(
     I            myTime, myIter, myThid)
C     *==================================================================*
C     | SUBROUTINE sgrunoff_read                                         |
C     *==================================================================*
C     | read 2-D subglacial runoff data from files,                      |
C     |      and fill the lowest layer of 3-D addmass variable           |
C     *==================================================================*

      IMPLICIT NONE

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "ICEFRONT.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif

c     == routine arguments ==
      _RL     myTime
      integer myIter
      integer myThid

#ifdef ALLOW_SUBGLACIAL_RUNOFF
c     == local variables ==
C     bi,bj,I,J,K :: loop counters
      INTEGER bi,bj,I,J,K

      call exf_set_runoff  (
     &     sgrunofffile, sgrunoffstartdate, sgrunoffperiod,
     &     exf_inscal_sgrunoff,
     &     sgrunoff_exfremo_intercept, sgrunoff_exfremo_slope,
     &     sgrunoff, sgrunoff0, sgrunoff1, sgrunoffmask,
     &     mytime, myiter, mythid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy,sNy+OLy
         DO I = 1-OLx,sNx+OLx
          K = kLowC(I,J,bi,bj)
          addMass(I,J,K,bi,bj) =
     &         addMass(I,J,K,bi,bj) + SGrunoff(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_SUBGLACIAL_RUNOFF */

      RETURN
      END
