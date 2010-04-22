C $Header: /u/gcmpack/MITgcm/pkg/autodiff/global_sum_tile_ad.F,v 1.3 2010/04/22 22:23:37 jmc Exp $
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

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     phiTile :: Input array with one value per tile
C     sumPhi  :: Result of sum.
C     myThid  :: My thread id.
      _RL     adphiTile(nSx,nSy)
      _RL     adsumPhi
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: tile indices
C     mpiRC  :: MPI return code
      INTEGER bi,bj
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
      CHARACTER*(MAX_LEN_MBUF) msgBuf

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
     &                   MPI_COMM_MODEL, mpiRC )
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

C---- Testing stage: print a warning (both to std err & outp) when
C     current Proc input value is different from Proc-0 value
      IF ( tmp.NE.adsumPhi ) THEN
C-    might need to improve this test if some MPI truncation happen
        WRITE(msgBuf,'(A,1PE22.14)')
     &    'GLOBAL_ADSUM_TILE_RL: ** WARNING ** input =', adsumPhi
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1PE22.14)')
     &    'GLOBAL_ADSUM_TILE_RL: ** WARNING ** output=', tmp
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
C----

      phiGSR8(1,0) = tmp

      _END_MASTER( myThid )

      _BARRIER

C--   every thread takes its adjoint sum
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         adphiTile(bi,bj) = phiGSR8(1,0)
       ENDDO
      ENDDO
C--   reset input to zero (jmc: is it right ? necessary ?)
c     adsumPhi = 0.

      RETURN
      END
