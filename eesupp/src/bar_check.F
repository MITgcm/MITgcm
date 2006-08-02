C $Header: /u/gcmpack/MITgcm/eesupp/src/bar_check.F,v 1.2 2006/08/02 02:24:22 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: BAR_CHECK

C     !INTERFACE:
      SUBROUTINE BAR_CHECK( barrierId, myThid )

C     !DESCRIPTION:
C     *=====================================================================*
C     | SUBROUTINE BAR\_CHECK
C     | o Check threads synchronization in the barrier calling sequence
C     *=====================================================================*
C     | o Apply double BARRIER and check that all threads get the same
C     |   barrierId.
C     *=====================================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
C     == Local common block ==
      INTEGER barStatus(nSx,nSy)
      COMMON / BAR_CHECH_SYNCHRO / barStatus

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     barrierId :: barrier identificator of this instance of BAR_CHECK
C     myThid    :: Thread number of this instance of BAR_CHECK
      INTEGER barrierId
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: tile indices
C     msgBuf :: Informational/error meesage buffer
      INTEGER bi,bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL flag
CEOP

      IF ( barrierId .NE. 0 ) THEN
C-    Only do checking when barrierId is non-zero

C-    Set barStatus to barrierId :
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          barStatus(bi,bj) = barrierId
        ENDDO
       ENDDO
C-    Synchro
       _BARRIER
C-    Check that all threads have the same barStatus
       flag = .FALSE.
       DO bj = 1,nSy
        DO bi = 1,nSx
          flag = flag .OR. (barStatus(bi,bj).NE.barrierId)
        ENDDO
       ENDDO
       IF ( flag ) THEN
         WRITE(msgBuf,'(A,I4,A,I8)') 'BAR_CHECK: thread', myThid,
     &        ' out of Sync when reaching barrierId=', barrierId
         CALL PRINT_ERROR( msgBuf, myThid )
#ifndef DISABLE_WRITE_TO_UNIT_ZERO
         WRITE(0,*) myThid, barrierId, 'barStatus=', barStatus
#endif
         STOP 'ABNORMAL END: S/R BAR_CHECK: OUT OF SYNC'
       ENDIF

      ENDIF
      
C-    Synchro
      _BARRIER

      RETURN
      END
