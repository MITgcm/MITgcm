#include "PTRACERS_OPTIONS.h"

CBOP 1
C     !ROUTINE: PTRACERS_DIAGNOSTICS_STATE

C     !INTERFACE:
      SUBROUTINE PTRACERS_DIAGNOSTICS_STATE(
     I                    uFld, vFld, wFld, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Fill-in the diagnostics array for PTRACERS state variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C     !INPUT PARAMETERS:
C     uFld    :: horizontal velocity, U component
C     vFld    :: horizontal velocity, V component
C     wFld    :: velocity vertical component
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number
C     myThid  :: my Thread Id number
      _RL     uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER i,j,k,bi,bj,iTr
      CHARACTER*8 diagName
      INTEGER km1

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName = '        '

      DO iTr = 1,PTRACERS_numInUse

        diagName = '        '
        WRITE(diagName,'(A4,A2)') 'TRAC',PTRACERS_ioLabel(iTr)
        CALL DIAGNOSTICS_FILL( pTracer(1-OLx,1-OLy,1,1,1,iTr), diagName,
     &                         0,Nr,0,1,1,myThid )

        diagName = '        '
        WRITE(diagName,'(A5,A2)') 'UTRAC',PTRACERS_ioLabel(iTr)
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,Nr
              DO j = 1,sNy
                DO i = 1,sNx+1
                  locVar(i,j,k,bi,bj) =
     &                 uFld(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i-1,j,k,bi,bj,iTr) )
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( locVar, diagName, 0,Nr,0,1,1,myThid )
        ENDIF

        diagName = '        '
        WRITE(diagName,'(A5,A2)') 'VTRAC',PTRACERS_ioLabel(iTr)
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,Nr
              DO j = 1,sNy+1
                DO i = 1,sNx
                  locVar(i,j,k,bi,bj) =
     &                 vFld(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i,j-1,k,bi,bj,iTr) )
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( locVar, diagName, 0,Nr,0,1,1,myThid )
        ENDIF

        diagName = '        '
        WRITE(diagName,'(A5,A2)') 'WTRAC',PTRACERS_ioLabel(iTr)
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           IF ( uniformFreeSurfLev ) THEN
            DO k=1,Nr
              km1 = MAX(k-1,1)
              DO j = 1,sNy
               DO i = 1,sNx
                 locVar(i,j,k,bi,bj) = wFld(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i,j,km1,bi,bj,iTr) )
               ENDDO
              ENDDO
            ENDDO
           ELSE
            DO k=1,Nr
              DO j = 1,sNy
               DO i = 1,sNx
                 km1 = MAX( k-1, MIN(kSurfC(i,j,bi,bj),Nr) )
                 locVar(i,j,k,bi,bj) = wFld(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i,j,km1,bi,bj,iTr) )
               ENDDO
              ENDDO
            ENDDO
           ENDIF
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( locVar, diagName, 0,Nr,0,1,1,myThid )
        ENDIF

C     end iTr loop
      ENDDO

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
