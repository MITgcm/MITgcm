C $Header: /u/gcmpack/MITgcm/pkg/runclock/runclock_init.F,v 1.1 2005/05/31 18:24:34 adcroft Exp $
C $Name:  $

#include "RUNCLOCK_OPTIONS.h"

CBOP
C     !ROUTINE: RUNCLOCK_INIT
C     !INTERFACE:
      SUBROUTINE RUNCLOCK_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE RUNCLOCK_INIT
C     | o Routine to initialize RUNCLOCK variables
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "RUNCLOCK.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_RUNCLOCK

C     !LOCAL VARIABLES:
C     === Local variables ===
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (.NOT.RUNCLOCKIsOn) THEN
       WRITE(msgBuf,'(A)') ' RUNCLOCK_INIT: RUNCLOCK not turned on'
       CALL PRINT_ERROR( msgBuf, myThid ) 
      ENDIF


C     Record the wall-clock time
      CALL runclock_gettime(
     &   RC_start_yr,RC_start_mo,RC_start_dy,
     &   RC_start_hr,RC_start_mi,RC_start_sc,
     &   RC_start_tins)

      CALL GLOBAL_MAX_R8(RC_start_tins,myThid)

      RC_prev_tins=RC_start_tins

c     write(0,*) 'RUNCLOCK_INIT: yr mo dy hr mi sc', 
c    &   RC_start_yr,RC_start_mo,RC_start_dy,
c    &   RC_start_hr,RC_start_mi,RC_start_sc,RC_start_tins

#endif /* ALLOW_RUNCLOCK */

      RETURN
      END
