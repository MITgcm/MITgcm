#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#include "W2_OPTIONS.h"

CBOP
C     !ROUTINE: EXCH2_CHECK_DEPTHS

C     !INTERFACE:
      SUBROUTINE EXCH2_CHECK_DEPTHS( rLow, rHigh, myThid )

C     !DESCRIPTION: \bc
C     *==========================================================*
C     | SUBROUTINE EXCH2_CHECK_DEPTHS
C     | o Check that disconnected tile edges (when using blank
C     |   tiles) correspond to a closed (= zero depth) boundary.
C     | Note: no check if using OBCs
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "EESUPPORT.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#ifdef ALLOW_OBCS
# include "PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     rLow    :: Lower  "r" boundary
C     rHigh   :: Higher "r" boundary
C     myThid  :: my Thread Id number
      _RS     rLow (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS     rHigh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEOP

C     == Local variables ==
      _RS     tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, tId
      INTEGER i, j, n
      INTEGER errN, errS, errE, errW
      LOGICAL errFlag

#ifdef ALLOW_OBCS
C-    For now, do nothing if OBCs is used
      IF ( useOBCs ) RETURN
#endif

      errFlag = .FALSE.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-    Fill E,W & N,S edges with total depth from the interior
         i = sNx+1
         DO j=1,sNy
           tmpFld(0,j) = rHigh( 1 ,j,bi,bj) - rLow( 1 ,j,bi,bj)
           tmpFld(i,j) = rHigh(sNx,j,bi,bj) - rLow(sNx,j,bi,bj)
         ENDDO
         j = sNy+1
         DO i=1,sNx
           tmpFld(i,0) = rHigh(i, 1 ,bi,bj) - rLow(i, 1 ,bi,bj)
           tmpFld(i,j) = rHigh(i,sNy,bi,bj) - rLow(i,sNy,bi,bj)
         ENDDO

C-    Reset to zero if connected
         tId = W2_myTileList(bi,bj)
         DO n= 1,exch2_nNeighbours(tId)
          DO j=exch2_jLo(n,tId),exch2_jHi(n,tId)
           DO i=exch2_iLo(n,tId),exch2_iHi(n,tId)
            tmpFld(i,j) = 0.
           ENDDO
          ENDDO
         ENDDO

C- North:
         errN = 0
         j = sNy+1
         DO i=1,sNx
           IF ( tmpFld(i,j).GT.0. ) errN = errN + 1
         ENDDO
C- South:
         errS = 0
         j = 0
         DO i=1,sNx
           IF ( tmpFld(i,j).GT.0. ) errS = errS + 1
         ENDDO
C- East :
         errE = 0
         i = sNx+1
         DO j=1,sNy
           IF ( tmpFld(i,j).GT.0. ) errE = errE + 1
         ENDDO
C- West :
         errW = 0
         i = 0
         DO j=1,sNy
           IF ( tmpFld(i,j).GT.0. ) errW = errW + 1
         ENDDO

         IF ( errN+errS+errW+errE .GE. 1 ) THEN
          WRITE(msgBuf,'(2A,I8,A,2(I4,A))')
     &        '** WARNING ** EXCH2_CHECK_DEPTHS: ',
     &        'tile #', tId, ' (bi,bj=', bi, ',', bj, ' ):'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          IF ( errN.GE.1 ) THEN
           WRITE(msgBuf,'(A,I5,A)') ' N.Edge has', errN,
     &        ' unconnected points with non-zero depth.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          IF ( errS.GE.1 ) THEN
           WRITE(msgBuf,'(A,I5,A)') ' S.Edge has', errS,
     &        ' unconnected points with non-zero depth.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          IF ( errE.GE.1 ) THEN
           WRITE(msgBuf,'(A,I5,A)') ' E.Edge has', errE,
     &        ' unconnected points with non-zero depth.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          IF ( errW.GE.1 ) THEN
           WRITE(msgBuf,'(A,I5,A)') ' W.Edge has', errW,
     &        ' unconnected points with non-zero depth.'
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          WRITE( msgBuf,'(A)') 'S/R EXCH2_CHECK_DEPTHS: Fatal Error'
          errFlag = .TRUE.
         ENDIF

       ENDDO
      ENDDO

#ifdef USE_ERROR_STOP
c     CALL STOP_IF_ERROR( errFlag, msgBuf, myThid )
#else  /* USE_ERROR_STOP */
c     IF ( errFlag ) STOP 'ABNORMAL END: S/R EXCH2_CHECK_DEPTHS'
#endif /* USE_ERROR_STOP */
      IF ( errFlag ) THEN
          WRITE( msgBuf,'(2A)') '** WARNING ** EXCH2_CHECK_DEPTHS:',
     &        ' some algorithm implementation might not be'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE( msgBuf,'(2A)') '** WARNING ** EXCH2_CHECK_DEPTHS:',
     &        ' safe with non-zero depth next to blank-tile'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
      ENDIF

      RETURN
      END
