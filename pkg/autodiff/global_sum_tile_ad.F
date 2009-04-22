C $Header: /u/gcmpack/MITgcm/pkg/autodiff/global_sum_tile_ad.F,v 1.1 2009/04/22 01:28:47 jmc Exp $
C $Name:  $

C--   File global_adsum.F: Routines that perform adjoint of
C                          global sum on an array of thread values.
C      Contents
C      o global_adsum_tile_rl
C      o global_adsum_tile_rs <- not yet coded
#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: GLOBAL_ADSUM_TILE_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_ADSUM_TILE_RL(
     O                        adPhiTile,
     I                        adsumPhi,
     I                        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_ADSUM\_TILE\_RL
C     | o Handle sum for _RL data.
C     *==========================================================*
C     | Apply sum on an array of one value per tile
C     |  and operate over all tiles & all the processes.
C     *==========================================================*

      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     == Routine arguments ==
C     phiTile :: Input array with one value per tile
C     sumPhi  :: Result of sum.
C     myThid  :: My thread id.
      _RL     adphiTile(nSx,nSy)
      _RL     adsumPhi
      INTEGER myThid
CEOP

C     == Local variables ==
C     bi,bj  :: tile indices
C     mpiRC  :: MPI return code
      INTEGER bi,bj
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      tmp = adsumPhi

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
         CALL MPI_Bcast( tmp, 1, MPI_DOUBLE_PRECISION, 0,
     &                   MPI_COMM_WORLD, mpiRC )
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

      shareBufGSR8(1,1) = tmp

      _END_MASTER( myThid )
C--
      _BARRIER

C--   every thread takes its adjoint sum
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         adphiTile(bi,bj) = shareBufGSR8(1,1)
       ENDDO
      ENDDO
C--   reset input to zero (jmc: is it right ? necessary ?)   
c     adsumPhi = 0.

      RETURN
      END
