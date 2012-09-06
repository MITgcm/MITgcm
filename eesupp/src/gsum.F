C $Header: /u/gcmpack/MITgcm/eesupp/src/gsum.F,v 1.9 2012/09/06 15:25:01 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CBOP

C     !ROUTINE: GSUM_R8_INIT

C     !INTERFACE:
      SUBROUTINE GSUM_R8_INIT( myThid )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GSUM\_R8\_INIT
C     | o Setup data structures for global sum.
C     *==========================================================*
C     | Fast true shared memory form for global sum operation.
C     *==========================================================*

C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C     GSR8_value :: Global data for accumulating sum elements.
C     GSR8_level :: Cyclic buffer index into global data sum elements.
      COMMON /GS_R8_BUFFER_R/
     &  GSR8_value
      Real*8 GSR8_value(lShare8,MAX_NO_THREADS)
#define _NOT_SET_ 1.23456D12
      COMMON /GS_R8_BUFFER_I/
     &  GSR8_level
      INTEGER GSR8_level

C     !INPUT PARAMETERS:
C     myThid :: Thread number of this instance.
      INTEGER myThid

C     !LOCAL VARIABLES:
C     I :: Loop counter.
      INTEGER I
CEOP
      GSR8_level = 1
      DO I = 1, lShare8
       GSR8_value(I,myThid) = _NOT_SET_
      ENDDO

      RETURN
      END

CBOP
C     !ROUTINE: GSUM_R8

C     !INTERFACE:
      SUBROUTINE GSUM_R8( myPhi, answer, myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GSUM\_R8
C     | o Perform global sum.
C     *==========================================================*
C     | Fast true shared memory form for global sum operation.
C     *==========================================================*
C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C     GSR8_value :: Global data for accumulating sum elements.
C     GSR8_level :: Cyclic buffer index into global data sum elements.
      COMMON /GS_R8_BUFFER_R/
     &  GSR8_value
      Real*8 GSR8_value(lShare8,MAX_NO_THREADS)
#define _NOT_SET_ 1.23456D12
      COMMON /GS_R8_BUFFER_I/
     &  GSR8_level
      INTEGER GSR8_level

C     !INPUT/OUTPUT PARAMETERS:
C     myPhi  :: This threads contribution
C     answer :: Result of sum over all threads
C     myThid :: This threads id number
      Real*8  myPhi
      Real*8  answer
      INTEGER myThid
#ifdef ALLOW_USE_MPI
      Real*8  tmp, sumPhi
      INTEGER mpiRc
#endif
C
C     !LOCAL VARIABLES:
C     nDone   :: Counter for number of threads completed.
C     I       :: Loop counter.
C     curLev  :: Cyclic global sum buffer levels.
C     prevLev
      INTEGER nDone
      INTEGER I
      INTEGER curLev, prevLev
CEOP

C     answer = 1.
C     CALL BAR2(myThid)
C     CALL BAR2(myThid)
C     CALL BAR2(myThid)
C     RETURN
C
      IF ( myThid .NE. 1 ) THEN

       curLev = GSR8_level
       GSR8_value(curLev,myThid) = myPhi
   10  CONTINUE
        IF ( GSR8_value(curLev,1) .NE. _NOT_SET_  ) GOTO 11
        CALL FOOL_THE_COMPILER_R8( GSR8_value(1,1) )
       GOTO 10
   11  CONTINUE
       GSR8_value(curLev,myThid) = _NOT_SET_
       answer = GSR8_value(curLev,1)

      ELSE

       curLev  = GSR8_level
       prevLev = curLev+1
       IF ( prevLev .GT. 2 ) prevLev = 1

   12  CONTINUE
        CALL FOOL_THE_COMPILER_R8( GSR8_value(1,1) )
        nDone   = 1
        DO I = 2, nThreads
         IF ( GSR8_value(curLev,I) .NE. _NOT_SET_ ) nDone = nDone+1
        ENDDO
       IF ( nDone .LT. nThreads ) GOTO 12

       GSR8_level = prevLev
       CALL FOOL_THE_COMPILER_R8( GSR8_value(1,1) )
       GSR8_value(prevLev,1) = _NOT_SET_

       answer = myPhi
       DO I = 2,nThreads
        answer = answer+GSR8_value(curLev,I)
       ENDDO

#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
        tmp = answer
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &                   MPI_COMM_MODEL,mpiRC)
        answer = sumPhi
       ENDIF
#endif /* ALLOW_USE_MPI */

       GSR8_value(curLev,1) = answer

      ENDIF

      RETURN
      END
