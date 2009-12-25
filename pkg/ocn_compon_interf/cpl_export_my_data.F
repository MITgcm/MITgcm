C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_export_my_data.F,v 1.3 2009/12/25 19:43:07 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_EXPORT_MY_DATA

C     !INTERFACE:
      SUBROUTINE CPL_EXPORT_MY_DATA(
     I                      myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_EXPORT_MY_DATA
C     | o Routine for controlling export of coupling data to
C     |   coupler layer.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*
C     \ev

C     !USES
      IMPLICIT NONE
C     == GLobal variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: Current time in simulation.
C     myIter  :: Current timestep number.
C     myThid  :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL OCN_STORE_MY_DATA(
     I           myTime, myIter, bi,bj, myThid )
       ENDDO
      ENDDO

C     Send data to coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous posting of data, so ordering
C     has to be consistent with coupling layer ordering.
      CALL OCN_EXPORT_FIELDS ( myThid )

#endif /* COMPONENT_MODULE */

      RETURN
      END
