C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_import_external_data.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_IMPORT_EXTERNAL_DATA(
     I           myCurrentIter, myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_IMPORT_EXTERNAL_DATA                      |
C     | o Routine for controlling import of coupling data from   |
C     |   coupler layer.                                         |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "CPL_PARAMS.h"

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

C     Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering 
C     has to be consistent with ordering coupling layer.
c     IF ( (myCurrentIter/96)*96 .EQ. myCurrentIter ) THEN
      IF ( (myCurrentIter/cplSendFrq_iter)*cplSendFrq_iter
     &      .EQ. myCurrentIter ) THEN
       WRITE(0,*) ' Importing fluxes at iteration ', myCurrentIter
       CALL ATM_IMPORT_SST (     myThid )
      ENDIF

      RETURN
      END
