C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_buoyancy.F,v 1.9 2010/08/12 21:49:55 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CALC_BUOYANCY(
     I      bi, bj, iMin, iMax, jMin, jMax,  k, rhoLoc,
     O      buoy,
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
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C--   == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      _RL rhoLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL buoy  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C--   == Local variables ==
      INTEGER i,j

      IF    ( buoyancyRelation .EQ. 'ATMOSPHERIC'  ) THEN

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=(theta(i,j,k,bi,bj)-tRef(k))/tRef(k)
        ENDDO
       ENDDO

      ELSEIF ( buoyancyRelation .EQ. 'OCEANIC' ) THEN

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=-gravity*recip_rhoConst*rholoc(i,j)
        ENDDO
       ENDDO

      ELSEIF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN

       DO j=jMin,jMax
        DO i=iMin,iMax
         if ( rholoc(i,j) .ne. 0. ) then
          rholoc(i,j) = 1./rholoc(i,j)
          buoy(i,j)=rholoc(i,j)
         endif
        ENDDO
       ENDDO

      ELSE

       STOP
     &'CALC_BUOANCY: variable "buoyancyRelation" has an illegal value'

      ENDIF

      RETURN
      END
