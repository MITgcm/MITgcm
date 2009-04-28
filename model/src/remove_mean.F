C $Header: /u/gcmpack/MITgcm/model/src/remove_mean.F,v 1.3 2009/04/28 18:30:33 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: REMOVE_MEAN_RL
C     !INTERFACE:
      SUBROUTINE REMOVE_MEAN_RL(
     I                myNr, arr, arrMask, arrhFac, arrArea, arrDr,
     I                arrName, myTime,
     I                myThid )
C     !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE REMOVE_MEAN_RL                                |
C     | o Calculate mean of global array "_RL arr" and substract |
C     |   it from the array                                      |
C     \==========================================================/
C     \ev

      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Routine arguments ===
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      _RL myTime
      INTEGER myThid

C#ifdef ALLOW_BALANCE_FLUXES
C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RL tmpVal
      _RL theMean
      _RL theVol
      _RL tmpVol
      CHARACTER*(max_len_mbuf) msgbuf
CEOP

      theMean=0.
      theVol=0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,myNr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=arr(I,J,K,bi,bj)
           IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
            tmpVol = arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
            theVol = theVol   + tmpVol
            theMean = theMean + tmpVol*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _GLOBAL_SUM_RL(theVol,myThid)
      _GLOBAL_SUM_RL(theMean,myThid)

      IF (theVol.GT.0.) THEN
       theMean=theMean/theVol

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,myNr
          DO J=1,sNy
           DO I=1,sNx
            IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
             arr(I,J,K,bi,bj) = arr(I,J,K,bi,bj) - theMean
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

C     Print the global mean to standard output, this is a measure for
C     the drift of the array arr
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgbuf,'(a,a,a,e24.17)')
     &      'REMOVE_MEAN_RL: Global mean of ',
     &      arrName, ' = ', theMean
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , 1)
       _END_MASTER( myThid )
      ENDIF

C#endif /* ALLOW_BALANCE_FLUXES */
      RETURN
      END

CBOP
C     !ROUTINE: REMOVE_MEAN_RS
C     !INTERFACE:
      SUBROUTINE REMOVE_MEAN_RS(
     I                myNr, arr, arrMask, arrhFac, arrArea, arrDr,
     I                arrName, myTime,
     I                myThid )
C     !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE REMOVE_MEAN_RS                                |
C     | o Calculate mean of global array "_RS arr" and substract |
C     |   it from the array                                      |
C     \==========================================================/
C     \ev

      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Routine arguments ===
      INTEGER myNr
      _RS arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      _RL myTime
      INTEGER myThid

C#ifdef ALLOW_BALANCE_FLUXES
C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RS tmpVal
      _RS theMean
      _RS theVol
      _RS tmpVol
      CHARACTER*(max_len_mbuf) msgbuf
CEOP

      theMean=0.
      theVol=0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,myNr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=arr(I,J,K,bi,bj)
           IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
            tmpVol = arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
            theVol = theVol   + tmpVol
            theMean = theMean + tmpVol*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _GLOBAL_SUM_RS(theVol,myThid)
      _GLOBAL_SUM_RS(theMean,myThid)

      IF (theVol.GT.0.) THEN
       theMean=theMean/theVol

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,myNr
          DO J=1,sNy
           DO I=1,sNx
            IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
             arr(I,J,K,bi,bj) = arr(I,J,K,bi,bj) - theMean
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

C     Print the global mean to standard output, this is a measure for
C     the drift of the array arr
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgbuf,'(a,a,a,e24.17)')
     &      'REMOVE_MEAN_RS: Global mean of ',
     &      arrName, ' = ', theMean
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , 1)
       _END_MASTER( myThid )
      ENDIF

C#endif /* ALLOW_BALANCE_FLUXES */
      RETURN
      END
