C $Header: /u/gcmpack/MITgcm/pkg/opps/opps_init.F,v 1.1 2004/09/16 11:28:16 mlosch Exp $
C $Name:  $

#include "OPPS_OPTIONS.h"

      SUBROUTINE OPPS_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE OPPS_INIT                                     |
C     | o Routine to initialize OPPS parameters and variables.   |
C     |==========================================================|
C     | Initialize OPPS parameters and variables.                |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OPPS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of MY_INIT
      INTEGER myThid

#ifdef ALLOW_OPPS

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj

C-----------------------------------------------------------------------
C Initialize OPPS variables 
C-----------------------------------------------------------------------

      do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        do k = 1, Nr
         do j = 1-OLy, sNy+OLy
          do i = 1-OLx, sNx+OLx
           OPPSconvectCount(I,J,K,bi,bj) = 0. _d 0
          end do
         end do
        end do
       end do
      end do

#endif /* ALLOW_OPPS */

      return
      end
