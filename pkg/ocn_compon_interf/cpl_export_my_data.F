C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_export_my_data.F,v 1.2 2004/05/21 19:42:26 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_EXPORT_MY_DATA( 
     I           myCurrentIter, myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_EXPORT_MY_DATA                            |
C     | o Routine for controlling export of coupling data to     |
C     |   coupler layer.                                         |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myCurrentIter - Current timestep number.
C     myCurrentTime - Current internal time.
C     myThid        - Thread number for this instance 
C                     of the routine.
      INTEGER myCurrentIter
      _RL     myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      INTEGER bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL OCN_STORE_MY_DATA( 
     I           myCurrentTime, myCurrentIter, bi,bj, myThid )
       ENDDO
      ENDDO

C     Send data to coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous posting of data, so ordering has to be
C     consistent with coupling layer ordering.
      CALL OCN_EXPORT_FIELDS ( myThid )

      RETURN
      END
