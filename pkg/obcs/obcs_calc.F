C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_calc.F,v 1.12 2005/10/10 05:53:48 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CALC( bi, bj, futureTime, futureIter,
     &                      uVel, vVel, wVel, theta, salt, 
     &                      myThid )
C     |==========================================================|
C     | SUBROUTINE OBCS_CALC                                     |
C     | o Calculate future boundary data at open boundaries      |
C     |   at time = futureTime                                   |
C     |==========================================================|
C     |                                                          |
C     |==========================================================|
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h" 
#include "OBCS.h"
#ifdef ALLOW_PTRACERS.h
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     == Routine arguments ==
      INTEGER bi, bj
      INTEGER futureIter
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
C     I,J,K        - loop indices
C     I_obc, J_obc - local index of open boundary
C     msgBuf       - Informational/error meesage buffer
      INTEGER I, J , K, I_obc, J_obc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_OBCS_BALANCE
      _RL Tr_T, Ar_T, Tr, Ar 
#endif /* ALLOW_OBCS_BALANCE */
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif /* ALLOW_PTRACERS */


#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_CALC',myThid)
#endif

#ifdef ALLOW_OBCS_EAST
C     Eastern OB
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_MSG('OBCS_CALC: East',myThid)
#endif
      IF (useOrlanskiEast) THEN
#ifdef ALLOW_ORLANSKI
        CALL ORLANSKI_EAST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
#endif
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Ie(J,bi,bj)
            IF (I_obc.ne.0) THEN
              OBEu(J,K,bi,bj)=0.
              OBEv(J,K,bi,bj)=0.
              OBEt(J,K,bi,bj)=tRef(K)
              OBEs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBEw(J,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBEeta(J,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_EAST */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_WEST
C     Western OB
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_MSG('OBCS_CALC: West',myThid)
#endif
      IF (useOrlanskiWest) THEN
#ifdef ALLOW_ORLANSKI
        CALL ORLANSKI_WEST(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
#endif
      ELSE
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Iw(J,bi,bj)
            IF (I_obc.ne.0) THEN
              OBWu(J,K,bi,bj)=0.
              OBWv(J,K,bi,bj)=0.
              OBWt(J,K,bi,bj)=tRef(K)
              OBWs(J,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBWw(J,K,bi,bj)=0.
#endif 
#ifdef NONLIN_FRSURF
              OBWeta(J,bi,bj)=0.
#endif
           ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_NORTH
C         Northern OB
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_MSG('OBCS_CALC: North',myThid)
#endif
      IF (useOrlanskiNorth) THEN
#ifdef ALLOW_ORLANSKI
        CALL ORLANSKI_NORTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
#endif
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Jn(I,bi,bj)
            IF (J_obc.ne.0) THEN
              OBNv(I,K,bi,bj)=0.
              OBNu(I,K,bi,bj)=0.
              OBNt(I,K,bi,bj)=tRef(K)
              OBNs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBNw(I,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBNeta(J,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_SOUTH
C         Southern OB
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_MSG('OBCS_CALC: South',myThid)
#endif
      IF (useOrlanskiSouth) THEN   
#ifdef ALLOW_ORLANSKI
        CALL ORLANSKI_SOUTH(
     &          bi, bj, futureTime, 
     &          uVel, vVel, wVel, theta, salt, 
     &          myThid )
#endif
      ELSE
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Js(I,bi,bj)
            IF (J_obc.ne.0) THEN
              OBSu(I,K,bi,bj)=0.
              OBSv(I,K,bi,bj)=0.
              OBSt(I,K,bi,bj)=tRef(K)
              OBSs(I,K,bi,bj)=sRef(K)
#ifdef ALLOW_NONHYDROSTATIC
              OBSw(I,K,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBSeta(J,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
C
C     Calculate some default open boundary conditions for passive tracers:
C     The default is a homogeneous v.Neumann conditions, that is, the 
C     tracer gradient across the open boundary is nearly zero; 
C     only nearly, because the boundary conditions are applied throughout 
C     the time step during which the interior field does change; therefore
C     we have to use the values from the previous time step here. If you 
C     really want exact v.Neumann conditions, you have to modify
C     obcs_apply_ptracer directly.
C
# ifdef ALLOW_OBCS_EAST
C     Eastern OB
#  ifdef ALLOW_DEBUG
       IF (debugMode)
     &      CALL DEBUG_MSG('OBCS_CALC: East, pTracers',myThid)
#  endif
       IF (useOrlanskiEast) THEN
#  ifdef ALLOW_ORLANSKI
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiEast Rad OBC with'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R OBCS_CALC'
#  endif
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I_obc=OB_Ie(J,bi,bj)
           IF (I_obc.ne.0) THEN
            OBEptr(J,K,bi,bj,iTracer) = 
     &           pTracer(I_obc-1,J,K,bi,bj,iTracer)
     &           *_maskW(I_obc,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* ALLOW_OBCS_EAST */

C ------------------------------------------------------------------------------

# ifdef ALLOW_OBCS_WEST
C     Western OB
#  ifdef ALLOW_DEBUG
       IF (debugMode) 
     &      CALL DEBUG_MSG('OBCS_CALC: West, pTracers',myThid)
#  endif
       IF (useOrlanskiWest) THEN
#  ifdef ALLOW_ORLANSKI
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiWest Rad OBC with'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R OBCS_CALC'
#  endif
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO K=1,Nr
          DO J=1-Oly,sNy+Oly
           I_obc=OB_Iw(J,bi,bj)
           IF (I_obc.ne.0) THEN
            OBWptr(J,K,bi,bj,iTracer) = 
     &           pTracer(I_obc+1,J,K,bi,bj,iTracer)
     &           *_maskW(I_obc+1,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* ALLOW_OBCS_WEST */

C ------------------------------------------------------------------------------

# ifdef ALLOW_OBCS_NORTH
C         Northern OB
#  ifdef ALLOW_DEBUG
       IF (debugMode) 
     &     CALL DEBUG_MSG('OBCS_CALC: North, pTracers',myThid)
#  endif
       IF (useOrlanskiNorth) THEN
#  ifdef ALLOW_ORLANSKI
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiNorth Rad OBC with'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R OBCS_CALC'
#  endif
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J_obc=OB_Jn(I,bi,bj)
           IF (J_obc.ne.0) THEN
            OBNptr(I,K,bi,bj,iTracer) =
     &           pTracer(I,J_obc-1,K,bi,bj,iTracer)
     &           *_maskS(I,J_obc,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* ALLOW_OBCS_NORTH */

C ------------------------------------------------------------------------------

# ifdef ALLOW_OBCS_SOUTH
C         Southern OB
# ifdef ALLOW_DEBUG
       IF (debugMode) 
     &      CALL DEBUG_MSG('OBCS_CALC: South, pTracers',myThid)
#endif
       IF (useOrlanskiSouth) THEN   
#ifdef ALLOW_ORLANSKI
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiSouth Rad OBC with'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R OBCS_CALC'
#endif
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
           J_obc=OB_Js(I,bi,bj)
           IF (J_obc.ne.0) THEN
            OBSptr(I,K,bi,bj,iTracer) = 
     &           pTracer(I,J_obc+1,K,bi,bj,iTracer)
     &           *_maskS(I,J_obc+1,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* ALLOW_OBCS_SOUTH */
C     end if (usePTracers)
      ENDIF	
#endif /* ALLOW_PTRACERS */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_PRESCRIBE
      IF (useOBCSprescribe) THEN
C--     Calculate future values on open boundaries
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('OBCS_PRESCRIBE_READ',myThid)
#endif
        CALL OBCS_PRESCRIBE_READ(futureTime, futureIter, mythid)
      ENDIF
#endif /* ALLOW_OBCS_PRESCRIBE */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_BALANCE
      IF ( useOBCSbalance) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_MSG('useOBCSbalance=.TRUE.',myThid)
#endif

#ifdef ALLOW_OBCS_EAST
        Tr_T = 0. _d 0
        Ar_T = 0. _d 0
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Ie(J,bi,bj)
            IF (I_obc.ne.0) THEN
              Ar = drF(k)*hFacC(I_obc,j,k,bi,bj)*dyG(I_obc,j,bi,bj)
              Ar_T = Ar_T + Ar 
              Tr_T = Tr_T + Ar * OBEu(J,K,bi,bj)
            ENDIF
          ENDDO
        ENDDO
        _GLOBAL_SUM_R8( Ar_T , myThid )
        _GLOBAL_SUM_R8( Tr_T , myThid )
        Tr_T = (0. - Tr_T)/Ar_T
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Ie(J,bi,bj)
            IF (I_obc.ne.0) THEN
              OBEu(J,K,bi,bj) = OBEu(J,K,bi,bj) + Tr_T
c              OBEv(J,K,bi,bj) = 0. 
            ENDIF
          ENDDO
        ENDDO
#endif

#ifdef ALLOW_OBCS_WEST
        Tr_T = 0. _d 0
        Ar_T = 0. _d 0
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Iw(J,bi,bj)
            IF (I_obc.ne.0) THEN
              Ar = drF(k)*hFacC(I_obc,j,k,bi,bj)*dyG(I_obc,j,bi,bj)
              Ar_T = Ar_T + Ar 
              Tr_T = Tr_T + Ar * OBWu(J,K,bi,bj)
            ENDIF
          ENDDO
        ENDDO
        _GLOBAL_SUM_R8( Ar_T , myThid )
        _GLOBAL_SUM_R8( Tr_T , myThid )
        Tr_T = (0. - Tr_T)/Ar_T
        DO K=1,Nr
          DO J=1-Oly,sNy+Oly
            I_obc=OB_Iw(J,bi,bj)
            IF (I_obc.ne.0) THEN
                OBWu(J,K,bi,bj) = OBWu(J,K,bi,bj) + Tr_T
c                OBWv(J,K,bi,bj) = 0. 
            ENDIF
          ENDDO
        ENDDO
#endif

#ifdef ALLOW_OBCS_NORTH
        Tr_T = 0. _d 0
        Ar_T = 0. _d 0
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Jn(I,bi,bj)
            IF (J_obc.ne.0) THEN
              Ar = drF(k)*hFacC(i,J_obc,k,bi,bj)*dxG(i,J_obc,bi,bj)
              Ar_T = Ar_T + Ar 
              Tr_T = Tr_T + Ar * OBNv(I,K,bi,bj)
            ENDIF
          ENDDO
        ENDDO
        _GLOBAL_SUM_R8( Ar_T , myThid )
        _GLOBAL_SUM_R8( Tr_T , myThid )
        Tr_T = (0. - Tr_T)/Ar_T
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Jn(I,bi,bj)
            IF (J_obc.ne.0) THEN
c                OBNu(I,K,bi,bj) = 0.
                OBNv(I,K,bi,bj) = OBNv(I,K,bi,bj) + Tr_T
            ENDIF
          ENDDO
        ENDDO
#endif

#ifdef ALLOW_OBCS_SOUTH
        Tr_T = 0. _d 0
        Ar_T = 0. _d 0
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Js(I,bi,bj)
            IF (J_obc.ne.0) THEN
              Ar = drF(k)*hFacC(i,J_obc,k,bi,bj)*dxG(i,J_obc,bi,bj)
              Ar_T = Ar_T + Ar 
              Tr_T = Tr_T + Ar * OBSv(I,K,bi,bj)
            ENDIF
          ENDDO
        ENDDO
        _GLOBAL_SUM_R8( Ar_T , myThid )
        _GLOBAL_SUM_R8( Tr_T , myThid )
        Tr_T = (0. - Tr_T)/Ar_T
        DO K=1,Nr
          DO I=1-Olx,sNx+Olx
            J_obc=OB_Js(I,bi,bj)
            IF (J_obc.ne.0) THEN
c                OBSu(I,K,bi,bj) = 0.
                OBSv(I,K,bi,bj) = OBSv(I,K,bi,bj) + Tr_T
            ENDIF
          ENDDO
        ENDDO
#endif

      ENDIF
#endif /* ALLOW_OBCS_BALANCE */

#endif /* ALLOW_OBCS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_CALC',myThid)
#endif
      RETURN
      END
