C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_write_tendency.F,v 1.3 2007/11/05 18:58:00 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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
c#include "PTRACERS_FIELDS.h"
#include "MATRIX.h"

      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MATRIX

      INTEGER bi,bj,i,j,k,iTracer,iRec
      CHARACTER*(MAX_LEN_MBUF) suff
      _RL recipImpMatrixCounter, recipExpDeltaTtracer

      DATA expMatrixWriteCount /0/
      DATA impMatrixWriteCount /0/

      IF ( (mod(myTime-startTime,expMatrixWriteTime)
     &     .EQ. (0.0 _d 0))) THEN
        recipExpDeltaTtracer =
     &       (1. _d 0)/(expMatrixCounter*dTtracerLev(1))
        IF (expMatrixWriteCount.EQ.0) expMatrixWriteCount=1
        iRec=expMatrixWriteCount
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid), myByHi (myThid)
            DO bi=myBxLo (myThid), myBxHi (myThid)
              DO k=1,Nr
                DO j=1-Oly, sNy+Oly
                  DO i=1-Olx, sNx+Olx
                    MATRIX(i,j,k,bi,bj,iTracer,1) =
     &                   MATRIX(i,j,k,bi,bj,iTracer,1)
     &                   *recipExpDeltaTtracer
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          WRITE(suff,'(A9,I2.2)') 'MATRIXEXP',iTracer
          write(*,*)'Writing explicit matrix :',iTracer,
     &         expMatrixWriteCount, expMatrixCounter
          CALL WRITE_REC_XYZ_RL(suff,
     &         MATRIX(1-Olx,1-Oly,1,1,1,iTracer,1),iRec,myIter,myThid)
        ENDDO
        expMatrixCounter=0
        expMatrixWriteCount=expMatrixWriteCount+1
C       Reset explicit matrix
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid), myByHi (myThid)
            DO bi=myBxLo (myThid), myBxHi (myThid)
              DO k=1,Nr
                DO j=1-Oly, sNy+Oly
                  DO i=1-Olx, sNx+Olx
                    MATRIX(i,j,k,bi,bj,iTracer,1)= 0. _d 0
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF ( (mod(myTime-startTime,impMatrixWriteTime)
     &     .EQ.(0.0 _d 0)) ) THEN
        recipImpMatrixCounter = (1. _d 0)/dble(impMatrixCounter)
        IF (impMatrixWriteCount.EQ.0) impMatrixWriteCount=1
        iRec=impMatrixWriteCount
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid), myByHi (myThid)
            DO bi=myBxLo (myThid), myBxHi (myThid)
              DO k=1,Nr
                DO j=1-Oly, sNy+Oly
                  DO i=1-Olx, sNx+Olx
                    MATRIX(i,j,k,bi,bj,iTracer,2) =
     &                   MATRIX(i,j,k,bi,bj,iTracer,2)
     &                   *recipImpMatrixCounter
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          WRITE(suff,'(A9,I2.2)') 'MATRIXIMP',iTracer
          write(*,*)'Writing implicit matrix :',iTracer,
     &         impMatrixWriteCount, impMatrixCounter
          CALL WRITE_REC_XYZ_RL(suff,
     &         MATRIX(1-Olx,1-Oly,1,1,1,iTracer,2),iRec,myIter,myThid)
        ENDDO
        impMatrixCounter=0
        impMatrixWriteCount=impMatrixWriteCount+1
C       Reset implicit matrix
        DO iTracer=1,PTRACERS_numInUse
          DO bj=myByLo(myThid), myByHi (myThid)
            DO bi=myBxLo (myThid), myBxHi (myThid)
              DO k=1,Nr
                DO j=1-Oly, sNy+Oly
                  DO i=1-Olx, sNx+Olx
                    MATRIX(i,j,k,bi,bj,iTracer,2)= 0. _d 0
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
