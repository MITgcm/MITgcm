C $Header: 
C $Name:  

#include "CPP_OPTIONS.h"

      SUBROUTINE REMOVE_MEAN_RL(
     I                myNr, arr, arrMask, arrhFac, arrArea, arrDr,
     I                arrName,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE REMOVE_MEAN_RL                                |
C     | o Calculate mean of global array "_RL arr" and substract |
C     |   it from the array                                      |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     === Routine arguments ===
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RL tmpVal
      _RL theMean
      _RL theVol
      _RL tmpVol
      CHARACTER*(max_len_mbuf) msgbuf

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

      _GLOBAL_SUM_R8(theVol,myThid)
      _GLOBAL_SUM_R8(theMean,myThid)

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
      WRITE(msgbuf,'(a,a,a,e24.17)') 
     &     '%GM: Global mean of ', arrName, ' = ', theMean
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)


      RETURN
      END
