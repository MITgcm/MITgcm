C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_variables.F,v 1.15 2005/12/14 16:43:19 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_INIT_VARIABLES( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_INIT_VARIABLES                           |
C     | o Initialise OBCs variable data                          |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "OBCS.h"
#ifdef ALLOW_PTRACERS.h
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER bi, bj
      INTEGER I, J, K
      CHARACTER*(10) suff
      INTEGER prec
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
CML      _RL ptracerLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_INIT_VARIABLES',myThid)
#endif

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO K=1,Nr
         DO I=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
          OBNu(I,K,bi,bj)=0. _d 0
          OBNv(I,K,bi,bj)=0. _d 0
          OBNt(I,K,bi,bj)=0. _d 0
          OBNs(I,K,bi,bj)=0. _d 0
# ifdef ALLOW_NONHYDROSTATIC
          OBNw(I,K,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
          OBNu0(I,K,bi,bj)=0. _d 0
          OBNv0(I,K,bi,bj)=0. _d 0
          OBNt0(I,K,bi,bj)=0. _d 0
          OBNs0(I,K,bi,bj)=0. _d 0
          OBNu1(I,K,bi,bj)=0. _d 0
          OBNv1(I,K,bi,bj)=0. _d 0
          OBNt1(I,K,bi,bj)=0. _d 0
          OBNs1(I,K,bi,bj)=0. _d 0
# endif
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
          OBSu(I,K,bi,bj)=0. _d 0
          OBSv(I,K,bi,bj)=0. _d 0
          OBSt(I,K,bi,bj)=0. _d 0
          OBSs(I,K,bi,bj)=0. _d 0
# ifdef ALLOW_NONHYDROSTATIC
          OBSw(I,K,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
          OBSu0(I,K,bi,bj)=0. _d 0
          OBSv0(I,K,bi,bj)=0. _d 0
          OBSt0(I,K,bi,bj)=0. _d 0
          OBSs0(I,K,bi,bj)=0. _d 0
          OBSu1(I,K,bi,bj)=0. _d 0
          OBSv1(I,K,bi,bj)=0. _d 0
          OBSt1(I,K,bi,bj)=0. _d 0
          OBSs1(I,K,bi,bj)=0. _d 0
# endif
#endif /* ALLOW_OBCS_SOUTH */
         ENDDO

         DO J=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
          OBEu(J,K,bi,bj)=0. _d 0
          OBEv(J,K,bi,bj)=0. _d 0
          OBEt(J,K,bi,bj)=0. _d 0
          OBEs(J,K,bi,bj)=0. _d 0
# ifdef ALLOW_NONHYDROSTATIC
          OBEw(J,K,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
          OBEu0(J,K,bi,bj)=0. _d 0
          OBEv0(J,K,bi,bj)=0. _d 0
          OBEt0(J,K,bi,bj)=0. _d 0
          OBEs0(J,K,bi,bj)=0. _d 0
          OBEu1(J,K,bi,bj)=0. _d 0
          OBEv1(J,K,bi,bj)=0. _d 0
          OBEt1(J,K,bi,bj)=0. _d 0
          OBEs1(J,K,bi,bj)=0. _d 0
# endif
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
          OBWu(J,K,bi,bj)=0. _d 0
          OBWv(J,K,bi,bj)=0. _d 0
          OBWt(J,K,bi,bj)=0. _d 0
          OBWs(J,K,bi,bj)=0. _d 0
# ifdef ALLOW_NONHYDROSTATIC
          OBWw(J,K,bi,bj)=0. _d 0
# endif
# ifdef ALLOW_OBCS_PRESCRIBE
          OBWu0(J,K,bi,bj)=0. _d 0
          OBWv0(J,K,bi,bj)=0. _d 0
          OBWt0(J,K,bi,bj)=0. _d 0
          OBWs0(J,K,bi,bj)=0. _d 0
          OBWu1(J,K,bi,bj)=0. _d 0
          OBWv1(J,K,bi,bj)=0. _d 0
          OBWt1(J,K,bi,bj)=0. _d 0
          OBWs1(J,K,bi,bj)=0. _d 0
# endif
#endif /* ALLOW_OBCS_WEST */
         ENDDO
        ENDDO

#ifdef ALLOW_PTRACERS
        DO iTracer=1,PTRACERS_numInUse
         DO K=1,Nr
          DO I=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
           OBNptr (I,K,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
           OBNptr0(I,K,bi,bj,iTracer)=0. _d 0
           OBNptr1(I,K,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
           OBSptr (I,K,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
           OBSptr0(I,K,bi,bj,iTracer)=0. _d 0
           OBSptr1(I,K,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_SOUTH */
          ENDDO

          DO J=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
           OBEptr (J,K,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
           OBEptr0(J,K,bi,bj,iTracer)=0. _d 0
           OBEptr1(J,K,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
           OBWptr (J,K,bi,bj,iTracer)=0. _d 0
# ifdef ALLOW_OBCS_PRESCRIBE
           OBWptr0(J,K,bi,bj,iTracer)=0. _d 0
           OBWptr1(J,K,bi,bj,iTracer)=0. _d 0
# endif
#endif /* ALLOW_OBCS_WEST */
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_PTRACERS */

#ifdef NONLIN_FRSURF
        DO I=1-Olx,sNx+Olx
          OBNeta(I,bi,bj)=0.
          OBSeta(I,bi,bj)=0.
        ENDDO
        DO J=1-Oly,sNy+Oly
          OBEeta(J,bi,bj)=0.
          OBWeta(J,bi,bj)=0.
        ENDDO
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_ORLANSKI
        IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &      useOrlanskiEast.OR.useOrlanskiWest) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('ORLANSKI_INIT',myThid)
#endif
          CALL ORLANSKI_INIT(bi, bj, myThid)
        ENDIF
#endif /* ALLOW_ORLANSKI */

       ENDDO
      ENDDO     

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C jmc: here is the logical place to read OBCS-pickup files
C      but a) without Orlanski: pass the test 1+1=2 without reading pickup.
C          b) with Orlanski: 1+1=2 fail even with this bit of code
      IF ( nIter0.NE.0 ) THEN
        prec = precFloat64
        IF (pickupSuff.EQ.' ') THEN
         WRITE(suff,'(I10.10)') nIter0
        ELSE
         WRITE(suff,'(A10)') pickupSuff
        ENDIF
c       CALL OBCS_READ_CHECKPOINT(prec, nIter0, suff, myThid)
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Apply OBCS values to initial conditions for consistancy
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('OBCS_CALC',myThid)
#endif
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL OBCS_CALC( bi, bj, startTime, nIter0,
     &        uVel, vVel, wVel, theta, salt, myThid )
       ENDDO
      ENDDO

      IF ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ' ) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) 
     &      CALL DEBUG_CALL('OBCS_APPLY_UV + OBCS_APPLY_TS',myThid)
#endif
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K=1,Nr
          CALL OBCS_APPLY_UV( bi, bj, k, uVel, vVel, myThid )
          CALL OBCS_APPLY_TS( bi, bj, k, theta, salt, myThid )
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_PTRACERS
C     repeat everything for passive tracers
       IF ( usePTRACERS ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) 
     &       CALL DEBUG_CALL('OBCS_APPLY_PTRACER',myThid)
#endif
        DO iTracer=1,PTRACERS_numInUse
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            CALL OBCS_APPLY_PTRACER( 
     I           bi, bj, K, iTracer, 
     U           ptracer(1-Olx,1-Oly,K,bi,bj,iTracer),
     I           myThid )
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_PTRACERS */
       IF (useOBCSprescribe) THEN
C     After applying the boundary conditions exchange the 3D-fields.
C     This is only necessary of the boudnary values have been read
C     from a file.
#ifdef ALLOW_DEBUG
        IF (debugMode) 
     &       CALL DEBUG_CALL('EXCHANGES in OBCS_INIT_VARIABLES',myThid)
#endif
        CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
        _EXCH_XYZ_R8( theta, myThid )
        _EXCH_XYZ_R8( salt , myThid )
       ENDIF
#ifdef ALLOW_PTRACERS
       IF (usePTRACERS) THEN
C     Loop over passive tracers
        DO iTracer=1,PTRACERS_numInUse
         _EXCH_XYZ_R8( pTracer(1-Olx,1-Oly,1,1,1,iTracer), 
     &        myThid )
        ENDDO
       ENDIF
#endif /* ALLOW_PTRACERS */
C     endif start from rest
      ENDIF

#endif /* ALLOW_OBCS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_INIT_VARIABLES',myThid)
#endif
      RETURN
      END
