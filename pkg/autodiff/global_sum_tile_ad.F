C $Header: /u/gcmpack/MITgcm/pkg/autodiff/global_sum_tile_ad.F,v 1.6 2011/03/29 23:09:39 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C--   File global_sum_tile_ad.F: Routines that perform adjoint of
C                          global sum on an array of thread values.
C      Contents
C      o GLOBAL_ADSUM_TILE_RL
C      o GLOBAL_ADSUM_TILE_RS <- not yet coded

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_ADSUM_TILE_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_ADSUM_TILE_RL(
     U                        adPhiTile,
     U                        adsumPhi,
     I                        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_ADSUM\_TILE\_RL
C     | o Adjoint version of global_sum_tile which returns
C     |   global sum over all tiles
C     | Note: Assume that adsumPhi is local to this thread
C     | (i.e., is not a shared var. and is not in a common block)
C     *==========================================================*
C     | Apply sum on an array of one value per tile
C     |  and operate over all tiles & all the processes.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     phiTile :: Input array with one value per tile
C     sumPhi  :: Result of sum.
C     myThid  :: My Thread Id.
      _RL     adphiTile(nSx,nSy)
      _RL     adsumPhi
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: tile indices
      INTEGER bi,bj
      Real*8  tmp

C note: arithmetic has been checked versus TAF generated adjoint
C       of a simplified version of global_sum_tile.F code.

      tmp = adsumPhi

      CALL GLOBAL_SUM_R8( tmp, myThid )

C--   each thread updates its tile adjoint sum
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         adphiTile(bi,bj) = adphiTile(bi,bj) + tmp
       ENDDO
      ENDDO

      adsumPhi = 0.

      RETURN
      END
