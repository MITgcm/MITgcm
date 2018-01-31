#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_DIAGNOSTICS_IMPL
C     !INTERFACE:
      SUBROUTINE GMREDI_DIAGNOSTICS_IMPL(
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_DIAGNOSTICS_IMPL
C     | o fill GM-Redi diagnostics for backward (implicit) terms
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL     DIAGNOSTICS_IS_ON
      EXTERNAL    DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */
CEOP

#ifdef ALLOW_GMREDI
#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
      _RL tmpVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( useDiagnostics .AND. implicitDiffusion .AND.
     &     Nr.GE.2 .AND. tempStepping ) THEN

       IF ( DIAGNOSTICS_IS_ON('GM_KwzTz', myThid ) ) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           CALL DIAGNOSTICS_COUNT( 'GM_KwzTz', bi, bj, myThid )
           DO k=2,Nr
             DO j = 1, sNy
              DO i = 1, sNx
                tmpVar(i,j) = -recip_drC(k)*rkSign
     &            *(theta(i,j,k,bi,bj)-theta(i,j,k-1,bi,bj))
     &            * maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
                tmpVar(i,j) = tmpVar(i,j)*Kwz(i,j,k,bi,bj)
     &                                   *rA(i,j,bi,bj)
              ENDDO
             ENDDO
             CALL DIAGNOSTICS_FILL( tmpVar, 'GM_KwzTz',
     &                              k, 1, 2, bi, bj, myThid )
           ENDDO
          ENDDO
         ENDDO
       ENDIF

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_GMREDI */

      RETURN
      END
