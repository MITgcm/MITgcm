#include "RBCS_OPTIONS.h"

CBOP
C !ROUTINE: RBCS_ADD_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_APPLY_ETA(bi, bj,
     U                          etaFld,
     i                          myTime, myIter,
     I                          myThid )

C !DESCRIPTION:
C     Add to tendency array the contribution from 3-D field relaxation

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif
#include "RBCS_SIZE.h"
#include "RBCS_PARAMS.h"
#include "RBCS_FIELDS.h"


C !INPUT/OUTPUT PARAMETERS: ============================================
C  bi,bj          :: tile indices
C  myTime         :: current time
C  myIter         :: current timestep
C  myThid         :: my Thread Id number
      INTEGER bi, bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL myTime
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  i,j            :: loop indices
      INTEGER i, j
      _RL rbcsVanishingFac
      _RL rec_tauRlx
CEOP

#ifdef ALLOW_RBCS

      IF (useRBCeta .AND. nonlinFreeSurf.GT.0) THEN

        IF ( rbcsVanishingTime.GT.0. _d 0 ) THEN
          rbcsVanishingFac =
     &        MAX( 0. _d 0 , 1. _d 0 - myTime / rbcsVanishingTime )
        ELSE
          rbcsVanishingFac = 1. _d 0
        ENDIF

        rec_tauRlx = rbcsVanishingFac/tauRelaxEta
        DO j=0,sNy+1
         DO i=0,sNx+1
          etaFld(i,j, bi, bj) = etaFld(i,j,bi,bj)
     &         - RBC_Etamask(i,j,bi,bj)*rec_tauRlx
     &          *( etaFld(i,j,bi,bj) - RBCeta(i,j,bi,bj) )

         ENDDO
        ENDDO
#ifdef ALLOW_DEBUG
        IF ( debugLevel.GE.debLevC ) THEN
          _BEGIN_MASTER( myThid )
          WRITE(msgBuf,'(A,1PE13.6,1PE13.6)')
     &    ' RBCS_APPLY_ETA: finished!', RBC_Etamask(sNx-10,1,bi,bj),
     &    RBCeta(sNx-10,1, bi,bj)
          CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
          _END_MASTER( myThid )
        ENDIF
#endif /* ALLOW_DEBUG */

      ENDIF
#endif /* ALLOW_RBCS */

      RETURN
      END
