C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_variables.F,v 1.23 2007/10/26 02:00:47 dimitri Exp $
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
#ifdef ALLOW_PTRACERS
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

#ifdef ALLOW_SEAICE
        DO I=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
         OBNa (I,bi,bj)=0. _d 0
         OBNh (I,bi,bj)=0. _d 0
         OBNa0(I,bi,bj)=0. _d 0
         OBNh0(I,bi,bj)=0. _d 0
         OBNa1(I,bi,bj)=0. _d 0
         OBNh1(I,bi,bj)=0. _d 0
         OBNsl (I,bi,bj)=0. _d 0
         OBNsn (I,bi,bj)=0. _d 0
         OBNsl0(I,bi,bj)=0. _d 0
         OBNsn0(I,bi,bj)=0. _d 0
         OBNsl1(I,bi,bj)=0. _d 0
         OBNsn1(I,bi,bj)=0. _d 0
         OBNuice (I,bi,bj)=0. _d 0
         OBNvice (I,bi,bj)=0. _d 0
         OBNuice0(I,bi,bj)=0. _d 0
         OBNvice0(I,bi,bj)=0. _d 0
         OBNuice1(I,bi,bj)=0. _d 0
         OBNvice1(I,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
         OBSa (I,bi,bj)=0. _d 0
         OBSh (I,bi,bj)=0. _d 0
         OBSa0(I,bi,bj)=0. _d 0
         OBSh0(I,bi,bj)=0. _d 0
         OBSa1(I,bi,bj)=0. _d 0
         OBSh1(I,bi,bj)=0. _d 0
         OBSsl (I,bi,bj)=0. _d 0
         OBSsn (I,bi,bj)=0. _d 0
         OBSsl0(I,bi,bj)=0. _d 0
         OBSsn0(I,bi,bj)=0. _d 0
         OBSsl1(I,bi,bj)=0. _d 0
         OBSsn1(I,bi,bj)=0. _d 0
         OBSuice (I,bi,bj)=0. _d 0
         OBSvice (I,bi,bj)=0. _d 0
         OBSuice0(I,bi,bj)=0. _d 0
         OBSvice0(I,bi,bj)=0. _d 0
         OBSuice1(I,bi,bj)=0. _d 0
         OBSvice1(I,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_SOUTH */
        ENDDO
        DO J=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
         OBEa (J,bi,bj)=0. _d 0
         OBEh (J,bi,bj)=0. _d 0
         OBEa0(J,bi,bj)=0. _d 0
         OBEh0(J,bi,bj)=0. _d 0
         OBEa1(J,bi,bj)=0. _d 0
         OBEh1(J,bi,bj)=0. _d 0
         OBEsl (I,bi,bj)=0. _d 0
         OBEsn (I,bi,bj)=0. _d 0
         OBEsl0(I,bi,bj)=0. _d 0
         OBEsn0(I,bi,bj)=0. _d 0
         OBEsl1(I,bi,bj)=0. _d 0
         OBEsn1(I,bi,bj)=0. _d 0
         OBEuice (I,bi,bj)=0. _d 0
         OBEvice (I,bi,bj)=0. _d 0
         OBEuice0(I,bi,bj)=0. _d 0
         OBEvice0(I,bi,bj)=0. _d 0
         OBEuice1(I,bi,bj)=0. _d 0
         OBEvice1(I,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
         OBWa (J,bi,bj)=0. _d 0
         OBWh (J,bi,bj)=0. _d 0
         OBWa0(J,bi,bj)=0. _d 0
         OBWh0(J,bi,bj)=0. _d 0
         OBWa1(J,bi,bj)=0. _d 0
         OBWh1(J,bi,bj)=0. _d 0
         OBWsl (I,bi,bj)=0. _d 0
         OBWsn (I,bi,bj)=0. _d 0
         OBWsl0(I,bi,bj)=0. _d 0
         OBWsn0(I,bi,bj)=0. _d 0
         OBWsl1(I,bi,bj)=0. _d 0
         OBWsn1(I,bi,bj)=0. _d 0
         OBWuice (I,bi,bj)=0. _d 0
         OBWvice (I,bi,bj)=0. _d 0
         OBWuice0(I,bi,bj)=0. _d 0
         OBWvice0(I,bi,bj)=0. _d 0
         OBWuice1(I,bi,bj)=0. _d 0
         OBWvice1(I,bi,bj)=0. _d 0
#endif /* ALLOW_OBCS_WEST */
        ENDDO
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_PTRACERS
#ifndef ALLOW_AUTODIFF_TAMC
        IF ( usePTRACERS ) THEN
#endif
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
#ifndef ALLOW_AUTODIFF_TAMC
        ENDIF
#endif
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

C--   Load/compute OBCS values in all cases, although these values are
C--   only used for initialisation and never for restart:
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
C--   Apply OBCS values to initial conditions for consistency
C      (but initial conditions only)
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
C     endif start from rest
      ENDIF
#ifdef ALLOW_PTRACERS
C     repeat everything for passive tracers
      IF ( usePTRACERS ) THEN
C     catch the case when we do start from a pickup for dynamics variables
C     but initialise ptracers differently
       IF ( nIter0 .EQ. PTRACERS_Iter0 ) THEN
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
        IF (useOBCSprescribe) THEN
C     After applying the boundary conditions exchange the 3D-fields.
C     This is only necessary of the boudnary values have been read
C     from a file.
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL( 
     &        'PTRACERS EXCHANGES in OBCS_INIT_VARIABLES',myThid)
#endif
         CALL PTRACERS_FIELDS_BLOCKING_EXCH( myThid )
        ENDIF
C     endif start from rest
       ENDIF
C     endif usePTRACERS
      ENDIF
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_OBCS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_INIT_VARIABLES',myThid)
#endif
      RETURN
      END
