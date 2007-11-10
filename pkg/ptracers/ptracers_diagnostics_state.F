C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_diagnostics_state.F,v 1.3 2007/11/10 22:09:32 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: PTRACERS_DIAGNOSTICS_STATE

C     !INTERFACE:
      SUBROUTINE PTRACERS_DIAGNOSTICS_STATE(myThid)

C     !DESCRIPTION:
C     Fill-in the diagnostics array for PTRACERS state variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL dummy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER i,j,k,bi,bj,iTr
      CHARACTER*8 diagName
      INTEGER km1

      diagName = '        '

      DO iTr = 1,PTRACERS_numInUse

        diagName = '        '
        WRITE(diagName,'(A4,A2)') 'TRAC',PTRACERS_ioLabel(iTr)
        CALL DIAGNOSTICS_FILL( pTracer(1-Olx,1-Oly,1,1,1,iTr), diagName,
     &                         0,Nr,0,1,1,myThid )

        diagName = '        '
        WRITE(diagName,'(A5,A2)') 'UTRAC',PTRACERS_ioLabel(iTr)
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,Nr
              DO j = 1,sNy
                DO i = 1,sNx+1
                  dummy(i,j,k,bi,bj) =
     &                 uVel(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i-1,j,k,bi,bj,iTr) )
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( dummy, diagName, 0,Nr,0,1,1,myThid )
        ENDIF

        diagName = '        '
        WRITE(diagName,'(A5,A2)') 'VTRAC',PTRACERS_ioLabel(iTr)
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,Nr
              DO j = 1,sNy+1
                DO i = 1,sNx
                  dummy(i,j,k,bi,bj) =
     &                 vVel(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i,j-1,k,bi,bj,iTr) )
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( dummy, diagName, 0,Nr,0,1,1,myThid )
        ENDIF

        diagName = '        '
        WRITE(diagName,'(A5,A2)') 'WTRAC',PTRACERS_ioLabel(iTr)
        IF ( DIAGNOSTICS_IS_ON(diagName,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k=1,Nr
              km1 = MAX(k-1,1)
              DO j = 1,sNy
                DO i = 1,sNx
                  dummy(i,j,k,bi,bj) = wVel(i,j,k,bi,bj) *
     &                 0.5 _d 0*( pTracer(i,j,k,bi,bj,iTr)
     &                          + pTracer(i,j,km1,bi,bj,iTr) )
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( dummy, diagName, 0,Nr,0,1,1,myThid )
        ENDIF

      ENDDO

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
