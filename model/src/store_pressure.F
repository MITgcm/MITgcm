C $Header: /u/gcmpack/MITgcm/model/src/Attic/store_pressure.F,v 1.1 2002/08/07 16:55:52 mlosch Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: STORE_PRESSURE
C     !INTERFACE:
      subroutine STORE_PRESSURE( 
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

      implicit none
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"

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
C     I,J
      INTEGER  I,  J

C              
C     compute the pressure for use in the equation of state
C
Cml      if ( buoyancyRelation .eq. 'OCEANIC' ) then
      if ( equationOfState .eq. 'JMD95P' 
     &         .or. equationOfState .eq. 'UNESCO' ) then
         do J=1-Oly,sNy+Oly
            do I=1-Olx,sNx+Olx
               pressure(i,j,k,bi,bj) = rhonil * ( 
     &              - gravity*rC(k)
     &              + gravity*etaN(i,j,bi,bj)
     &              + phiHyd(i,j,k)
     &                                           )
            end do
         end do
      endif
Cml      elseif ( buoyancyRelation .eq. 'ATMOSPHERIC' ) then
CmlC     in pressure coordinates the pressure is just the coordinate of 
CmlC     the tracer point
Cml         do J=1-Oly,sNy+Oly
Cml            do I=1-Olx,sNx+Olx
Cml               pressure(i,j,k,bi,bj) = rC(k)
Cml            end do
Cml         end do
Cml      endif	

      return
      end
