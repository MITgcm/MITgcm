C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_buoyancy.F,v 1.2 1998/09/06 14:47:43 cnh Exp $

#include "CPP_EEOPTIONS.h"

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
C     | kRef - determines pressure reference level               |
C     |        (not used in 'LINEAR' mode)                       |
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
      INTEGER kRef	
      _RL rhoLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL buoy  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C--   == Local variables ==
      INTEGER i,j
      _RL refTemp,refSalt,sigRef,tP,sP,deltaSig
! ------------------------------------------------------------------------------

      IF    ( buoyancyRelation .eq. 'ATMOSPHERIC'  ) THEN

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=-specVol_S(K)/theta_S(K)*(theta(i,j,k,bi,bj)-tRef(K))
        ENDDO
       ENDDO
       
      ELSEIF ( buoyancyRelation .eq. 'OCEANIC' ) THEN

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=-Gravity*recip_rhoConst*rholoc(i,j)
        ENDDO
       ENDDO

      ELSE

       STOP 'CALC_BUOANCY: variable "buoyancyRelation" has an illegal value'

      ENDIF


      RETURN
      END
