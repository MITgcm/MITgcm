C $Header: /u/gcmpack/MITgcm/pkg/pp81/Attic/pp81_init.F,v 1.1 2004/09/02 09:11:54 mlosch Exp $
C $Name:  $

#include "PP81_OPTIONS.h"

      SUBROUTINE PP81_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE PP81_INIT                                     |
C     | o Routine to initialize PP81 parameters and variables.   |
C     |==========================================================|
C     | Initialize PP81 parameters and variables.                |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PP81.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of PP81_INIT
      INTEGER myThid

#ifdef ALLOW_PP81

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj

C-----------------------------------------------------------------------
C Initialize PP variables PPviscAr, PPdiffKrT
C-----------------------------------------------------------------------

      do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        do k = 1, Nr
         do j = 1-OLy, sNy+OLy
          do i = 1-OLx, sNx+OLx
           PPviscAr (i,j,k,bi,bj) = viscAr
           PPdiffKr (i,j,k,bi,bj) = diffKrT
          end do
         end do
        end do
       end do
      end do

#endif /* ALLOW_PP81 */

      return
      end
