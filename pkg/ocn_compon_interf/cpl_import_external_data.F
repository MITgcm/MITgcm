C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_import_external_data.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
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

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

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

C     Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering 
C     has to be consistent with ordering coupling layer.
      CALL OCN_IMPORT_TAUX (         myThid )
      CALL OCN_IMPORT_TAUY (         myThid )
      CALL OCN_IMPORT_HEATFLUX (     myThid )
      CALL OCN_IMPORT_FWFLUX (       myThid )
      CALL OCN_IMPORT_UVELGROUND (   myThid )
      CALL OCN_IMPORT_VVELGROUND (   myThid )
      CALL OCN_IMPORT_QLATENT (      myThid )
      CALL OCN_IMPORT_QSENSIBLE (    myThid )
      CALL OCN_IMPORT_QLONGWAVE (    myThid )
      CALL OCN_IMPORT_QSHORTWAVE (   myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        CALL CPL_CALC_DIAGS( bi, bj, myCurrentTime, myThid )
       ENDDO
      ENDDO

      RETURN
      END
