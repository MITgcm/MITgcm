#include "CPP_EEOPTIONS.h"

      SUBROUTINE GSUM_R8_INIT( myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C
      COMMON /GS_R8_BUFFER_R/
     &  GSR8_value
      Real*8 GSR8_value(lShare8,MAX_NO_THREADS)
#define _NOT_SET_ 1.23456D12
      COMMON /GS_R8_BUFFER_I/
     &  GSR8_level
      INTEGER GSR8_level
C
      INTEGER myThid
      INTEGER I
C
      GSR8_level = 1
      DO I = 1, lShare8
       GSR8_value(I,myThid) = _NOT_SET_
      ENDDO
C
      RETURN
      END
      SUBROUTINE GSUM_R8( myPhi, answer, myThid )
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C
      Real*8  myPhi
      Real*8  answer
      INTEGER myThid
#ifdef ALLOW_USE_MPI
      Real*8  tmp, sumPhi
      INTEGER mpiRc
#endif
C
      INTEGER nDone
      INTEGER I
      INTEGER curLev, prevLev
C
      COMMON /GS_R8_BUFFER_R/
     &  GSR8_value
      Real*8 GSR8_value(lShare8,MAX_NO_THREADS)
#define _NOT_SET_ 1.23456D12
      COMMON /GS_R8_BUFFER_I/
     &  GSR8_level
      INTEGER GSR8_level

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
        CALL FOOL_THE_COMPILER( GSR8_value )
       GOTO 10
   11  CONTINUE
       GSR8_value(curLev,myThid) = _NOT_SET_
       answer = GSR8_value(curLev,1)

      ELSE

       curLev  = GSR8_level
       prevLev = curLev+1
       IF ( prevLev .GT. 2 ) prevLev = 1

   12  CONTINUE
        CALL FOOL_THE_COMPILER( GSR8_value )
        nDone   = 1
        DO I = 2, nThreads
         IF ( GSR8_value(curLev,I) .NE. _NOT_SET_ ) nDone = nDone+1
        ENDDO
       IF ( nDone .LT. nThreads ) GOTO 12

       GSR8_level = prevLev
       CALL FOOL_THE_COMPILER( GSR8_value )
       GSR8_value(prevLev,1) = _NOT_SET_

       answer = myPhi
       DO I = 2,nThreads
        answer = answer+GSR8_value(curLev,I)
       ENDDO

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif
        tmp = answer
        CALL MPI_Allreduce(tmp,sumPhi,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &                   MPI_COMM_MODEL,mpiRC)
        answer = sumPhi
#ifndef ALWAYS_USE_MPI
       ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

       GSR8_value(curLev,1) = answer

      ENDIF
C
      RETURN
      END
