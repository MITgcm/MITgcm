#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif /* ALLOW_CTRL */

CBOP
C     !ROUTINE: OBCS_CALC

C     !INTERFACE:
      SUBROUTINE OBCS_CALC( futureTime, futureIter,
     &                      uVel, vVel, wVel, theta, salt,
     &                      myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_CALC
C     | o Calculate future boundary data at open boundaries
C     |   at time = futureTime
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
# include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_NEST_CHILD
# include "NEST_CHILD.h"
#endif /* ALLOW_NEST_CHILD */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER futureIter
      _RL futureTime
      _RL uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS

C     !LOCAL VARIABLES:
C     bi, bj       :: tile indices
C     i,j,k        :: loop indices
C     I_obc, J_obc :: local index of open boundary
C     msgBuf       :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, k
#ifdef ALLOW_PTRACERS
      INTEGER I_obc, J_obc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iTracer
#endif /* ALLOW_PTRACERS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_CALC',myThid)
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_NEST_CHILD
      IF ( useNEST_CHILD ) THEN
        IF ( PASSI.LT.2 ) THEN
         CALL NEST_CHILD_RECV ( myThid )
        ENDIF
      ENDIF
#endif /* ALLOW_NEST_CHILD */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
#ifdef ALLOW_NEST_CHILD
      ELSEIF ( useNEST_CHILD ) THEN
        DO k=1,Nr
          DO j=1-OLy,sNy+OLy
            IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
              OBEu(j,k,bi,bj)= U_F1(j,k,2)
              OBEv(j,k,bi,bj)= V_F1(j,k,2)
              OBEt(j,k,bi,bj)= T_F1(j,k,2)
              OBEs(j,k,bi,bj)= S_F1(j,k,2)
#ifdef NONLIN_FRSURF
              OBEeta(j,bi,bj)= ETA_F1(j,1,2)
#endif
            ENDIF
          ENDDO
        ENDDO
#endif /* ALLOW_NEST_CHILD */
      ELSE
        DO k=1,Nr
          DO j=1-OLy,sNy+OLy
            IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
              OBEu(j,k,bi,bj)=0.
              OBEv(j,k,bi,bj)=0.
              OBEt(j,k,bi,bj)=tRef(k)
              OBEs(j,k,bi,bj)=sRef(k)
#ifdef ALLOW_NONHYDROSTATIC
              OBEw(j,k,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBEeta(j,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_EAST */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
#ifdef ALLOW_NEST_CHILD
      ELSEIF ( useNEST_CHILD ) THEN
        DO k=1,Nr
          DO j=1-OLy,sNy+OLy
            IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
              OBWu(j,k,bi,bj)= U_F1(j,k,1)
              OBWv(j,k,bi,bj)= V_F1(j,k,1)
              OBWt(j,k,bi,bj)= T_F1(j,k,1)
              OBWs(j,k,bi,bj)= S_F1(j,k,1)
#ifdef NONLIN_FRSURF
              OBWeta(j,bi,bj)= ETA_F1(j,1,1)
#endif
           ENDIF
          ENDDO
        ENDDO
#endif /* ALLOW_NEST_CHILD */
      ELSE
        DO k=1,Nr
          DO j=1-OLy,sNy+OLy
            IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
              OBWu(j,k,bi,bj)=0.
              OBWv(j,k,bi,bj)=0.
              OBWt(j,k,bi,bj)=tRef(k)
              OBWs(j,k,bi,bj)=sRef(k)
#ifdef ALLOW_NONHYDROSTATIC
              OBWw(j,k,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBWeta(j,bi,bj)=0.
#endif
           ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
        DO k=1,Nr
          DO i=1-OLx,sNx+OLx
            IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
              OBNv(i,k,bi,bj)=0.
              OBNu(i,k,bi,bj)=0.
              OBNt(i,k,bi,bj)=tRef(k)
              OBNs(i,k,bi,bj)=sRef(k)
#ifdef ALLOW_NONHYDROSTATIC
              OBNw(i,k,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBNeta(i,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
        DO k=1,Nr
          DO i=1-OLx,sNx+OLx
            IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
              OBSu(i,k,bi,bj)=0.
              OBSv(i,k,bi,bj)=0.
              OBSt(i,k,bi,bj)=tRef(k)
              OBSs(i,k,bi,bj)=sRef(k)
#ifdef ALLOW_NONHYDROSTATIC
              OBSw(i,k,bi,bj)=0.
#endif
#ifdef NONLIN_FRSURF
              OBSeta(i,bi,bj)=0.
#endif
            ENDIF
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiEast Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CALC'
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
            I_obc = OB_Ie(j,bi,bj)
            OBEptr(j,k,bi,bj,iTracer) =
     &           pTracer(I_obc-1,j,k,bi,bj,iTracer)
     &           *_maskW(I_obc,j,k,bi,bj)
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
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiWest Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CALC'
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
            I_obc = OB_Iw(j,bi,bj)
            OBWptr(j,k,bi,bj,iTracer) =
     &           pTracer(I_obc+1,j,k,bi,bj,iTracer)
     &           *_maskW(I_obc+1,j,k,bi,bj)
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
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiNorth Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CALC'
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO k=1,Nr
          DO i=1-OLx,sNx+OLx
           IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
            J_obc = OB_Jn(i,bi,bj)
            OBNptr(i,k,bi,bj,iTracer) =
     &           pTracer(i,J_obc-1,k,bi,bj,iTracer)
     &           *_maskS(i,J_obc,k,bi,bj)
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
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: useOrlanskiSouth Rad OBC with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'OBCS_CALC: ERROR: pTracers not yet implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R OBCS_CALC'
       ELSE
        DO iTracer=1,PTRACERS_numInUse
         DO k=1,Nr
          DO i=1-OLx,sNx+OLx
           IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
            J_obc = OB_Js(i,bi,bj)
            OBSptr(i,k,bi,bj,iTracer) =
     &           pTracer(i,J_obc+1,k,bi,bj,iTracer)
     &           *_maskS(i,J_obc+1,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDIF
# endif /* ALLOW_OBCS_SOUTH */
C     end if (usePTracers)
      ENDIF
#endif /* ALLOW_PTRACERS */

C--   end bi,bj loops.
      ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_OBCS_PRESCRIBE
      IF (useOBCSprescribe) THEN
C--     Calculate future values on open boundaries
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OBCS_PRESCRIBE_READ',myThid)
#endif
        CALL OBCS_PRESCRIBE_READ( futureTime, futureIter, myThid )
      ENDIF
#endif /* ALLOW_OBCS_PRESCRIBE */

C ------------------------------------------------------------------------------
C     read control parameter contributions here to be independent of flags
C     ALLOW_OBCS_PRESCRIBE and useOBCSprescribe
#ifdef ALLOW_CTRL
      IF ( useCTRL ) THEN
# ifdef ALLOW_OBCSN_CONTROL
       CALL CTRL_GETOBCSN ( futureTime, futureIter, mythid )
# endif
# ifdef ALLOW_OBCSS_CONTROL
       CALL CTRL_GETOBCSS ( futureTime, futureIter, mythid )
# endif
# ifdef ALLOW_OBCSW_CONTROL
       CALL CTRL_GETOBCSW ( futureTime, futureIter, myThid )
# endif
# ifdef ALLOW_OBCSE_CONTROL
       CALL CTRL_GETOBCSE ( futureTime, futureIter, myThid )
# endif
      ENDIF
#endif /* ALLOW_CTRL */

C ------------------------------------------------------------------------------

#ifdef ALLOW_OBCS_STEVENS
C     The Stevens (1990) boundary conditions come after reading data from
C     files, because they use the data to compute a mix of simplified
C     Orlanski and prescribed boundary conditions
      IF (useStevensNorth.OR.useStevensSouth.OR.
     &     useStevensEast.OR.useStevensWest) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('OBCS_CALC_STEVENS',myThid)
#endif
       CALL OBCS_CALC_STEVENS( futureTime, futureIter, myThid )
      ENDIF
#endif /* ALLOW_OBCS_STEVENS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_CALC',myThid)
#endif
#endif /* ALLOW_OBCS */

      RETURN
      END
