C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_init_variables.F,v 1.3 2001/02/28 14:45:21 adcroft Exp $
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

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == Local variables ==
      INTEGER bi, bj
      INTEGER I, J, K

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO K=1,Nr
         DO I=1-Olx,sNx+Olx
          OBNu(I,K,bi,bj)=0.
          OBNv(I,K,bi,bj)=0.
          OBNt(I,K,bi,bj)=0.
          OBSu(I,K,bi,bj)=0.
          OBSv(I,K,bi,bj)=0.
          OBSt(I,K,bi,bj)=0.
#ifdef ALLOW_NONHYDROSTATIC
          OBNw(I,K,bi,bj)=0.
          OBSw(I,K,bi,bj)=0.
#endif
         ENDDO
         DO J=1-Oly,sNy+Oly
          OBEu(J,K,bi,bj)=0.
          OBEv(J,K,bi,bj)=0.
          OBEt(J,K,bi,bj)=0.
          OBWu(J,K,bi,bj)=0.
          OBWv(J,K,bi,bj)=0.
          OBWt(J,K,bi,bj)=0.
#ifdef ALLOW_NONHYDROSTATIC
          OBEw(J,K,bi,bj)=0.
          OBWw(J,K,bi,bj)=0.
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_ORLANSKI
        IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &      useOrlanskiEast.OR.useOrlanskiWest) THEN
          CALL ORLANSKI_INIT(bi, bj, myThid)
        ENDIF
#endif /* ALLOW_ORLANSKI */

       ENDDO
      ENDDO     

C--   Apply OBCS values to initial conditions for consistancy
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL OBCS_CALC( bi, bj, startTime,
     &        uVel, vVel, wVel, theta, salt, myThid )
        DO K=1,Nr
         CALL OBCS_APPLY_UV( bi, bj, k, uVel, vVel, myThid )
         CALL OBCS_APPLY_TS( bi, bj, k, theta, salt, myThid )
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS */
      RETURN
      END
