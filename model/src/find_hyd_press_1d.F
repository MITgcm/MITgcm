#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FIND_HYD_PRESS_1D
C     !INTERFACE:
      SUBROUTINE FIND_HYD_PRESS_1D(
     O                    pCen, pInt,
     U                    rhoCen,
     I                    tCen, sCen, maxResid,
     I                    belowCritNb, maxIterNb, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FIND_HYD_PRESS_1D
C     | o Over one column, find pressure in hydrostatic balance
C     |   with updated density (which is function of pressure)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     pCen      :: Pressure vertical profile at grid-cell center
C     pInt      :: Pressure vertical profile at grid-cell interface
C     rhoCen    :: Density vertical profile
C     tCen      :: Potential temperature vertical profile
C     sCen      :: Salinity/water-vapor vertical profile
C     maxResid  :: Maximum density difference criteria
C   belowCritNb :: Required number of consecutive iter below maxResid
C     maxIterNb :: Maximum number of iterations to perform
C     myThid    :: my Thread Id number
      _RL     pCen(Nr), pInt(Nr+1)
      _RL     rhoCen(Nr)
      _RL     tCen(Nr), sCen(Nr)
      _RL     maxResid
      INTEGER belowCritNb, maxIterNb
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     k, n       :: loop counter
C     nIter      :: iteration counter
C     nUnderCrit :: count number of iter below density Diff Criteria
C     searchForP :: Continue to iterate/search for P(rho(P))
C     rhoLoc     :: new density profile (computed using new Pres)
C     diffMax    :: Maximum density difference (new minus previous)
C     diffRMS    :: Root Mean Squared density difference
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k, n
      INTEGER nIter, nUnderCrit
      LOGICAL searchForP
      _RL     rhoLoc(Nr)
      _RL     dRlocM, dRlocP, dRho
      _RL     diffMax, diffRMS
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A,I5,A)') 'FIND_HYD_PRESS_1D: ',
     &  'Start to iterate (MaxIter=', maxIterNb,
     &   ' ) until P(rho(P))'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A,I3,A,1PE13.6)') 'FIND_HYD_PRESS_1D: ',
     &   ' converges ; critera (x', belowCritNb,
     &   ') on Rho diff=', maxResid
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      pInt(1)  = top_Pres
      searchForP = .TRUE.
      nUnderCrit = 0
      DO n=1,maxIterNb
       IF ( searchForP ) THEN
        nIter = n

C--   Integrate Hydrostatic pressure
        IF ( integr_GeoPot.EQ.1 ) THEN
         DO k=1,Nr
          pCen(k)   = pInt(k)
     &              + rhoCen(k)*gravity*gravFacC(k)*drF(k)*halfRL
          pInt(k+1) = pInt(k)
     &              + rhoCen(k)*gravity*gravFacC(k)*drF(k)
         ENDDO
        ELSE
         DO k=1,Nr
          dRlocM = halfRL*drC(k)
          dRlocP = halfRL*drC(k+1)
          IF (k.EQ.1)  dRlocM = drC(k)
          IF (k.EQ.Nr) dRlocP = drC(k+1)
          pCen(k)   = pInt(k)
     &              + rhoCen(k)*gravity*gravFacF(k)*dRlocM
          pInt(k+1) = pCen(k)
     &              + rhoCen(k)*gravity*gravFacF(k+1)*dRlocP
         ENDDO
        ENDIF

C--   Compute new density
        DO k=1,Nr
          CALL FIND_RHO_SCALAR(
     I                          tCen(k), sCen(k), pCen(k),
     O                          rhoLoc(k), myThid )
        ENDDO

C--   Test for convergence:
        diffRMS = 0.
        diffMax = 0.
        DO k=1,Nr
          dRho = rhoLoc(k)-rhoCen(k)
          IF ( ABS(dRho) .GT. ABS(diffMax) ) diffMax = dRho
          diffRMS = diffRMS + dRho*dRho
        ENDDO
        diffRMS = diffRMS/DFLOAT(Nr)
        IF ( diffRMS.GT.0. ) diffRMS = SQRT(diffRMS)
        IF ( ABS(diffMax).LE.maxResid ) THEN
          nUnderCrit = nUnderCrit + 1
        ELSE
          nUnderCrit = 0
        ENDIF
C-    Double criteria: stop if perfect convergence or if below
C      criteria for at least "belowCritNb" iterations
        searchForP = ( nUnderCrit.LT.belowCritNb )
     &         .AND. ( diffMax.NE.zeroRL )

        IF ( debugLevel.GE.debLevB ) THEN
         WRITE(msgBuf,'(A,I5,2(A,1PE20.12))')
     &    ' iter', nIter,', RMS-diff=', diffRMS,', Max-diff=', diffMax
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF

        IF ( searchForP .AND. nIter.LT.maxIterNb ) THEN
C--   Update rhoCen for new iteration
         DO k=1,Nr
          rhoCen(k) = rhoLoc(k)
         ENDDO
        ENDIF

       ENDIF
      ENDDO

      IF ( searchForP ) THEN
        WRITE(msgBuf,'(2A,I5,A,I3,A)') 'FIND_HYD_PRESS_1D: ',
     &    'No convergence after', nIter,
     &    ' iters (nUnderCrit=', nUnderCrit, ' )'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R FIND_HYD_PRESS_1D'
      ELSE
        WRITE(msgBuf,'(2A,I5,A,I3,A)') 'FIND_HYD_PRESS_1D: ',
     &    'converged after', nIter,
     &    ' iters (nUnderCrit=', nUnderCrit, ' )'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C--   Update rhoCen with new solution
        DO k=1,Nr
          rhoCen(k) = rhoLoc(k)
        ENDDO
      ENDIF

      RETURN
      END
