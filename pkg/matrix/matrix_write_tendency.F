C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_write_tendency.F,v 1.5 2012/04/02 21:26:12 jmc Exp $
C $Name:  $

#include "MATRIX_OPTIONS.h"

C !ROUTINE MATRIX_WRITE_TENDENCY.F
C This routine writes both the explicit and implicit matrices
C to file.

      SUBROUTINE MATRIX_WRITE_TENDENCY( myTime, myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "MATRIX.h"

      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MATRIX

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

      INTEGER bi,bj,i,j,k,iTracer,iRec
      CHARACTER*(MAX_LEN_FNAM) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL recipImpMatrixCounter, recipExpDeltaTtracer

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( DIFFERENT_MULTIPLE(expMatrixWriteTime,myTime,deltaTClock)
     &   ) THEN
        bj=myByLo(myThid)
        bi=myBxLo(myThid)
        recipExpDeltaTtracer = expMatrixCounter(bi,bj)*dTtracerLev(1)
        recipExpDeltaTtracer = 1. _d 0 / recipExpDeltaTtracer
        iRec = expMatrixWriteCount
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo (myThid),myBxHi(myThid)
             DO k=1,Nr
              DO j=1-OLy, sNy+OLy
               DO i=1-OLx, sNx+OLx
                 MATRIX(i,j,k,bi,bj,iTracer,1) =
     &                   MATRIX(i,j,k,bi,bj,iTracer,1)
     &                   *recipExpDeltaTtracer
               ENDDO
              ENDDO
             ENDDO
           ENDDO
          ENDDO
          _BEGIN_MASTER(myThid)
          bj=myByLo(myThid)
          bi=myBxLo(myThid)
          WRITE(msgBuf,'(A,I4,2I6)') 'Writing explicit matrix :',
     &       iTracer, expMatrixWriteCount, expMatrixCounter(bi,bj)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          _END_MASTER(myThid)
          WRITE(suff,'(A9,I2.2)') 'MATRIXEXP',iTracer
          CALL WRITE_REC_XYZ_RL(suff,
     &         MATRIX(1-OLx,1-OLy,1,1,1,iTracer,1),iRec,myIter,myThid)
        ENDDO
        _BARRIER
        _BEGIN_MASTER(myThid)
         expMatrixWriteCount = expMatrixWriteCount + 1
        _END_MASTER(myThid)

C       Reset explicit matrix
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo (myThid),myBxHi(myThid)
           expMatrixCounter(bi,bj) = 0
         ENDDO
        ENDDO
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo (myThid),myBxHi(myThid)
             DO k=1,Nr
              DO j=1-OLy, sNy+OLy
               DO i=1-OLx, sNx+OLx
                 MATRIX(i,j,k,bi,bj,iTracer,1) = 0. _d 0
               ENDDO
              ENDDO
             ENDDO
           ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF ( DIFFERENT_MULTIPLE(impMatrixWriteTime,myTime,deltaTClock)
     &   ) THEN
        bj=myByLo(myThid)
        bi=myBxLo(myThid)
        recipImpMatrixCounter = impMatrixCounter(bi,bj)
        recipImpMatrixCounter = 1. _d 0 / recipImpMatrixCounter
        iRec = impMatrixWriteCount
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid), myByHi (myThid)
           DO bi=myBxLo (myThid), myBxHi (myThid)
             DO k=1,Nr
              DO j=1-OLy, sNy+OLy
               DO i=1-OLx, sNx+OLx
                 MATRIX(i,j,k,bi,bj,iTracer,2) =
     &                   MATRIX(i,j,k,bi,bj,iTracer,2)
     &                   *recipImpMatrixCounter
               ENDDO
              ENDDO
             ENDDO
           ENDDO
          ENDDO
          _BEGIN_MASTER(myThid)
          bj=myByLo(myThid)
          bi=myBxLo(myThid)
          WRITE(msgBuf,'(A,I4,2I6)') 'Writing implicit matrix :',
     &       iTracer, impMatrixWriteCount, impMatrixCounter(bi,bj)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          _END_MASTER(myThid)
          WRITE(suff,'(A9,I2.2)') 'MATRIXIMP',iTracer
          CALL WRITE_REC_XYZ_RL(suff,
     &         MATRIX(1-OLx,1-OLy,1,1,1,iTracer,2),iRec,myIter,myThid)
        ENDDO
        _BARRIER
        _BEGIN_MASTER(myThid)
         impMatrixWriteCount = impMatrixWriteCount + 1
        _END_MASTER(myThid)

C       Reset implicit matrix
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo (myThid),myBxHi(myThid)
           impMatrixCounter(bi,bj) = 0
         ENDDO
        ENDDO
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid), myByHi (myThid)
           DO bi=myBxLo (myThid), myBxHi (myThid)
             DO k=1,Nr
              DO j=1-OLy, sNy+OLy
               DO i=1-OLx, sNx+OLx
                 MATRIX(i,j,k,bi,bj,iTracer,2) = 0. _d 0
               ENDDO
              ENDDO
             ENDDO
           ENDDO
          ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_MATRIX */
      RETURN
      END
