C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_import_external_data.F,v 1.2 2004/05/21 19:42:26 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_IMPORT_EXTERNAL_DATA(
     I           myIter, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_IMPORT_EXTERNAL_DATA                      |
C     | o Routine for controlling import of coupling data from   |
C     |   coupler layer.                                         |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myIter - Current timestep number.
C     myTime - Current internal time.
C     myThid - Thread number for this instance of the routine.
      INTEGER myIter
      _RL     myTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      INTEGER bi,bj

C-    Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering 
C     has to be consistent with ordering coupling layer.
      CALL OCN_IMPORT_FIELDS ( myThid )

C-    Modify forcing according to Imported fields from coupler
c     CALL OCN_APPLY_IMPORT( myTime, myIter, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        CALL OCN_CPL_DIAGS( bi, bj, myTime, myThid )
       ENDDO
      ENDDO

      RETURN
      END
