C $Header: /u/gcmpack/MITgcm/model/src/Attic/quasihydrostaticterms.F,v 1.3 2005/04/06 18:20:22 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE QUASIHYDROSTATICTERMS(
     I      bi,bj,k,
     O      effectiveBuoy,
     I      myThid )

C     /==========================================================\
C     | o SUBROUTINE BUOYANCY                                    |
C     |   Calculates buoyancy for an XY slice of a tile.         |
C     |==========================================================|
C     |                                                          |
C     | k - is the density level                                 |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C--   == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C--   == Routine arguments ==
      INTEGER bi,bj,k
      _RL effectiveBuoy(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C--   == Local variables ==
      INTEGER i,j
      _RL scalingFactor

      scalingFactor=rhoConst/gravity
      scalingFactor=1./gravity

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        effectiveBuoy(i,j)=effectiveBuoy(i,j)
     &    -scalingFactor*
     &     fCoriCos(i,j,bi,bj)*
     &     0.5*( uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj) )
       ENDDO
      ENDDO

      IF (useNHMTerms) THEN
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         effectiveBuoy(i,j)=effectiveBuoy(i,j)
     &    -scalingFactor*
     &     (
     &       0.5*( uVel(i,j,k,bi,bj)**2+uVel(i+1,j,k,bi,bj)**2 )
     &      +0.5*( vVel(i,j,k,bi,bj)**2+vVel(i,j+1,k,bi,bj)**2 )
     &     )*recip_RSphere
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
