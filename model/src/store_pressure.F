C $Header: /u/gcmpack/MITgcm/model/src/Attic/store_pressure.F,v 1.7 2003/02/09 02:00:50 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: STORE_PRESSURE
C     !INTERFACE:
      SUBROUTINE STORE_PRESSURE( 
     I     bi, bj, k, phiHyd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE store_pressure                                      
C     | o Compute the absolute pressure for the 
C     |   full nonlinear equation of state 
C     |   and store it to a common block
C     *==========================================================*
C     \ev

C     !USES:

      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  - Loop counters
C     k      - vertical loop counter
C     myThid - Thread number for this instance of the routine.
      INTEGER bi, bj, K
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: loop index
      INTEGER  i,j
CEOP

C
C     compute the pressure for use in the equation of state
C
      IF ( buoyancyRelation .EQ. 'OCEANIC' .AND.
     &     ( equationOfState .EQ. 'JMD95P'
     &       .OR. equationOfState .EQ. 'UNESCO' 
     &       .OR. equationOfState .EQ. 'MDJWF' ) ) THEN
         DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
               pressure(i,j,k,bi,bj) = rhoConst * (
     &              - gravity*rC(k)
     &              + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &              + phiHyd(i,j,k)
     &              + phi0surf(i,j,bi,bj)
     &                                           )
     &              * maskC(i,j,k,bi,bj)
            ENDDO
         ENDDO
      ELSEIF ( buoyancyRelation .EQ. 'ATMOSPHERIC'
     &    .OR. buoyancyRelation .EQ. 'OCEANICP' ) THEN
C     in pressure coordinates the pressure is just the coordinate of
C     the tracer point
         DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
               pressure(i,j,k,bi,bj) = rC(k)
     &              * maskC(i,j,k,bi,bj)
            ENDDO
         ENDDO
      ENDIF

      RETURN 
      END
