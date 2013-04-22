C $Header: /u/gcmpack/MITgcm/model/src/Attic/external_forcing_adjust.F,v 1.1 2013/04/22 02:38:07 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#undef CHECK_OVERLAP_FORCING

CBOP
C     !ROUTINE: EXTERNAL_FORCING_ADJUST
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_ADJUST(
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_ADJUST
C     *==========================================================*
C     | o Apply adjustment (balancing forcing) and exchanges
C     |   to oceanic surface forcing
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime   :: Current simulation time for this instance.
C     myIter   :: Current time-step number
C     myThid   :: my Thread Id number
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
#ifdef CHECK_OVERLAP_FORCING
      INTEGER bi, bj
      INTEGER ii, i, j
#endif
CEOP

#ifdef ALLOW_BALANCE_FLUXES
C     balance fluxes
      IF ( balanceEmPmR .AND. (.NOT.useSeaice .OR. useThSIce) )
     &      CALL REMOVE_MEAN_RS( 1, EmPmR, maskInC, maskInC, rA, drF,
     &        'EmPmR', myTime, myThid )
      IF ( balanceQnet  .AND. (.NOT.useSeaice .OR. useThSIce) )
     &      CALL REMOVE_MEAN_RS( 1, Qnet,  maskInC, maskInC, rA, drF,
     &        'Qnet ', myTime, myThid )
#endif /* ALLOW_BALANCE_FLUXES */

C     Apply exchanges (if needed)

#ifdef CHECK_OVERLAP_FORCING
C     Put large value in overlap of forcing array to check exch is needed
c     IF ( .NOT. useKPP ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

         DO j=1-OLy,sNy+OLy
          IF ( j.LT.1 .OR. j.GT.sNy ) THEN
           DO i=1-OLx,sNx+OLx
             EmPmR(i,j,bi,bj) = 1.
             Qnet(i,j,bi,bj)  = 400.
             Qsw(i,j,bi,bj)   = -200.
             saltFlux(i,j,bi,bj) = 40.
           ENDDO
          ELSE
           DO ii=1-OLx,OLx
             i = ii
             IF ( ii.GE.1 ) i= ii+sNx
             EmPmR(i,j,bi,bj) = 1.
             Qnet(i,j,bi,bj)  = 400.
             Qsw(i,j,bi,bj)   = -200.
             saltFlux(i,j,bi,bj) = 40.
           ENDDO
          ENDIF
         ENDDO
C-   end bi,bj loops
        ENDDO
       ENDDO
c     ENDIF
#endif /* CHECK_OVERLAP_FORCING */

      RETURN
      END
