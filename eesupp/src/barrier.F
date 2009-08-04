C $Header: /u/gcmpack/MITgcm/eesupp/src/barrier.F,v 1.14 2009/08/04 18:01:37 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: BARRIER_INIT

C     !INTERFACE:
      SUBROUTINE BARRIER_INIT
      IMPLICIT NONE

C     !DESCRIPTION:
C     *=====================================================================*
C     | SUBROUTINE BARRIER\_INIT
C     | o Setup global barrier data structures.
C     *=====================================================================*
C     | Initialise global barrier data structures that can be used in
C     | conjunction with MPI or that can also be used to create
C     *=====================================================================*

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BARRIER.h"

C     !LOCAL VARIABLES:
C     == Local Variables ==
C     I :: Loop counter
      INTEGER I
CEOP

      DO I=1,nThreads
       key1(1,I) = INVALID
       key2(1,I) = INVALID
       key3(1,I) = INVALID
       door1     = SHUT
       door2     = SHUT
       door3     = SHUT
       bCount(I) = 0
       masterSet(I) = 0
      ENDDO

      RETURN
      END

CBOP
C     !ROUTINE: BARRIER

C     !INTERFACE:
      SUBROUTINE BARRIER( myThid )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE BARRIER
C     | o Barrier routine that uses "busy waiting".
C     *==========================================================*
C     | This routine provides a pure fortran mechanism to
C     | synchronise multiple threads in a multi-threaded code.
C     | No thread can leave this routine before all the threads
C     | have entered it.
C     | Notes
C     | =====
C     | The door and key variables are assumed to have been
C     | initialized once an initial state of key = INVALID
C     | and door = SHUT.
C     | We use the routine FOOL\_THE\_COMPILER to stop compilers
C     | generating code which might simply set and test a
C     | register value. Shared-memory systems only maintain
C     | coherency over process caches and not registers.
C     | Also we have to be a bit careful regarding sequential
C     | consistency - or lack of it. At the moment the code
C     | assumes a total store order memory model, which some
C     | machines do not have! However, I have yet to find a
C     | problem with this I think because the tolerances in
C     | terms of memory ordering i.e. a little bit of reordering
C     | probably will not break the barrier mechanism!
C     | On non-cache coherent systems e.g. T3E we need to use
C     | a library function to do barriers.
C     | Note - The PANIC tests can be removed for working code
C     |        I have left them in without an ifdef option
C     |        because without them programming errors can
C     |        lead to infinitely spinning code. If you are
C     |        confident that your code is OK then removing
C     |        them may increase performance. Do not remove these
C     |        lines to make your code "work" If the code is
C     |        stopping in these PANIC blocks then something is
C     |        wrong with your program and it needs to be fixed.
C     *==========================================================*

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BARRIER.h"

C     !INPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     nDone :: Counter for number of threads that have
C              completed a section.
C     I     :: Loop counter
      INTEGER nDone
      INTEGER I
CEOP

CcnhDebugStarts
C      WRITE(myThid,*) ' Barrier entered '
CcnhDebugEnds

#ifdef USE_OMP_THREADING
C$OMP BARRIER
      bCount(myThid) = bCount(myThid) + 1
      IF ( masterSet(myThid) .NE. 0 ) THEN
       PRINT *, 'BARRIER called for master reg myThid == ',
     &   myThid, masterSet(myThid)
      ENDIF
Cdbg C$OMP BARRIER
Cdbg       DO I=2, nThreads
Cdbg        IF (bCount(I) .NE. bCount(1) ) THEN
Cdbg          PRINT *, bCount(1:nThreads)
Cdbg          CALL SYSTEM('sleep 1')
Cdbg          PRINT *, bCount(1:nThreads)
Cdbg          PRINT *, bCount(1:nThreads)
Cdbg          PRINT *, bCount(1:nThreads)
Cdbg          PRINT *, bCount(1:nThreads)
Cdbg          STOP ' barrier out of sync '
Cdbg        ENDIF
Cdbg       ENDDO
Cdbg C$OMP BARRIER
      RETURN
#endif

C--   Check that thread number is expected range
      IF ( myThid .LT. 1 .OR. myThid .GT. nThreads ) THEN
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! CATASTROPHIC ERROR'
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! in S/R BARRIER  myThid = ',
     &  myThid, ' nThreads = ', nThreads
       STOP 'ABNROMAL END: S/R BARRIER'
      ENDIF

C--   When every threads key1 is valid thread 1 will open door1.
      IF ( key1(1,myThid) .EQ. VALID ) THEN
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! CATASTROPHIC ERROR'
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! in S/R BARRIER  myThid = ',
     &  myThid, ' key1 already validated'
       STOP 'ABNROMAL END: S/R BARRIER'
      ENDIF
      key1(1,myThid) = VALID

      IF ( myThid .eq. 1 ) THEN
   10  CONTINUE
        nDone = 0
        DO I=1,nThreads
         if ( key1(1,I) .EQ. VALID ) nDone = nDone+1
        ENDDO
        CALL FOOL_THE_COMPILER( key1(1,1) )
       IF ( nDone .LT. nThreads ) GOTO 10
       door1 = OPEN
      ELSE
  11   CONTINUE
        CALL FOOL_THE_COMPILER( door1 )
       IF ( door1 .NE. OPEN ) GOTO 11
      ENDIF
C--   Invalidate keys for door1 here as it is now open
      key1(1,myThid) = INVALID

CcnhDebugStarts
C     IF ( myThid .EQ. 1 ) THEN
C      WRITE(*,*) ' DOOR1 Opened '
C     ENDIF
CcnhDebugEnds

C--   I can now shut door3 because I know everyone has reached
C--   door1. I can not shut door1 because I do not know if everyone
C--   has "gone" through the door yet. Nobody has yet reached
C--   door3 because they have to go through door2 first.
      IF ( myThid .EQ. 1 ) THEN
       door3 = SHUT
      ENDIF

C--   When every threads key2 is valid thread 1 will open door2.
C     Notes
C     =====
C     I think that to work with any memory model ( i.e. relaxed,
C     partial store, total store) the variables key1, key2 and key3
C     might need to be set to invalid by thread 1.
C
      IF ( key2(1,myThid) .EQ. VALID ) THEN
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! CATASTROPHIC ERROR'
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! in S/R BARRIER  myThid = ',
     &  myThid, ' key2 already validated'
       STOP 'ABNROMAL END: S/R BARRIER'
      ENDIF
      key2(1,myThid) = VALID
C
      IF ( myThid .eq. 1 ) THEN
   20  CONTINUE
        nDone = 0
        DO I=1,nThreads
         if ( key2(1,I) .EQ. VALID ) nDone = nDone+1
        ENDDO
        CALL FOOL_THE_COMPILER( key2(1,1) )
       IF ( nDone .LT. nThreads ) GOTO 20
       door2 = OPEN
      ELSE
  21   CONTINUE
        CALL FOOL_THE_COMPILER( door2 )
       IF ( door2 .NE. OPEN ) GOTO 21
      ENDIF

C--   Invalidate keys for door2 here as it is now open
      key2(1,myThid) = INVALID

C--   I can now shut door1 because I know everyone has reached
C--   door2. I can not shut door2 because I do not know if everyone
C--   has "gone" through the door yet. Nobody has yet reached
C--   door1 because they have to go through door3 first.
      IF ( myThid .EQ. 1 ) THEN
       door1 = SHUT
      ENDIF


C--   When every threads key3 is valid thread 1 will open door3.
      IF ( key3(1,myThid) .EQ. VALID ) THEN
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! CATASTROPHIC ERROR'
       WRITE(*,*) '!!!!!!! PANIC !!!!!!! in S/R BARRIER  myThid = ',
     &  myThid, ' key3 already validated'
       STOP 'ABNROMAL END: S/R BARRIER'
      ENDIF
      key3(1,myThid) = VALID
C
      IF ( myThid .eq. 1 ) THEN
   30  CONTINUE
        nDone = 0
        DO I=1,nThreads
         if ( key3(1,I) .EQ. VALID ) nDone = nDone+1
        ENDDO
        CALL FOOL_THE_COMPILER( key3(1,1) )
       IF ( nDone .LT. nThreads ) GOTO 30
       door3 = OPEN
      ELSE
  31   CONTINUE
        CALL FOOL_THE_COMPILER( door3 )
       IF ( door3 .NE. OPEN ) GOTO 31
      ENDIF

C--   Invalidate keys for door3 here as it is now open
      key3(1,myThid) = INVALID

C--   I can now shut door2 because I know everyone has reached
C--   door3. I can not shut door3 because I do not know if everyone
C--   has "gone" through the door yet. Nobody has yet reached
C--   door2 because they have to go through door1 first.
      IF ( myThid .EQ. 1 ) THEN
       door2 = SHUT
      ENDIF

CcnhDebugStarts
C      WRITE(myThid,*) ' Barrier exited '
CcnhDebugEnds

      RETURN
      END

CBOP
      SUBROUTINE BARRIER_MS( myThid )
      IMPLICIT NONE

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BARRIER.h"
      INTEGER myThid

      masterSet(myThid) = masterSet(myThid) + 1

      RETURN
      END
      SUBROUTINE BARRIER_MU( myThid )
      IMPLICIT NONE

C     !USES:
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "BARRIER.h"
      INTEGER myThid

      masterSet(myThid) = masterSet(myThid) - 1

      RETURN
      END
