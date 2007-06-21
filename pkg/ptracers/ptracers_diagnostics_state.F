C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_diagnostics_state.F,v 1.1 2007/06/21 00:32:21 jmc Exp $
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
#include "PTRACERS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid 
CEOP

#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL dummy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nr,Nsx,Nsy)
      INTEGER i,j,K,N,bi,bj
      CHARACTER*8 diagname
      INTEGER km1
      
      diagname = '        '
      
c     DO N = 1,PTRACERS_numInUse
C-    due to format (I2) and diagnostics name limitation, 
C      => do not go beyong 99 tracers
      DO N = 1,MIN(99,PTRACERS_numInUse)
        
        diagname = '        '
        WRITE(diagname,'(A4,I2.2)') 'TRAC',N
        CALL DIAGNOSTICS_FILL( ptracer(1-Olx,1-Oly,1,1,1,N), diagname,
     &                         0,Nr,0,1,1,myThid )
        
        diagname = '        '
        WRITE(diagname,'(A5,I2.2)') 'UTRAC',N
        IF ( DIAGNOSTICS_IS_ON(diagname,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              DO j = 1,sNy
                DO i = 1,sNx
                  dummy(i,j,K,bi,bj) = 
     &                 uVel(i,j,K,bi,bj)*hFacW(i,j,K,bi,bj) *
     &                 0.5 _d 0*(ptracer(i,j,K,bi,bj,N)
     &                       + ptracer(i-1,j,K,bi,bj,N))
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( dummy, diagname, 0,Nr,0,1,1,myThid )
        ENDIF
        
        diagname = '        '
        WRITE(diagname,'(A5,I2.2)') 'VTRAC',N
        IF ( DIAGNOSTICS_IS_ON(diagname,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              DO j = 1,sNy
                DO i = 1,sNx
                  dummy(i,j,K,bi,bj) = 
     &                 vVel(i,j,K,bi,bj)*hFacS(i,j,K,bi,bj) *
     &                 0.5 _d 0*(ptracer(i,j,K,bi,bj,N)
     &                       + ptracer(i,j-1,K,bi,bj,N))
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( dummy, diagname, 0,Nr,0,1,1,myThid )
        ENDIF
        
        diagname = '        '
        WRITE(diagname,'(A5,I2.2)') 'WTRAC',N
        IF ( DIAGNOSTICS_IS_ON(diagname,myThid) ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO K=1,Nr
              km1 = MAX(K-1,1)
              DO j = 1,sNy
                DO i = 1,sNx
                  dummy(i,j,K,bi,bj) = wVel(i,j,K,bi,bj) *
     &                 0.5 _d 0*(ptracer(i,j,K,bi,bj,N)
     &                       + ptracer(i,j,km1,bi,bj,N))
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL( dummy, diagname, 0,Nr,0,1,1,myThid )
        ENDIF
        
      ENDDO

#endif /* ALLOW_DIAGNOSTICS */

      RETURN 
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
