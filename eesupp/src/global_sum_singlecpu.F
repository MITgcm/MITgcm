C $Header: /u/gcmpack/MITgcm/eesupp/src/global_sum_singlecpu.F,v 1.5 2012/09/03 19:36:29 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

C--   File global_sum_singlecpu.F: Routines that perform global sum
C                                  on a single CPU
C      Contents
C      o GLOBAL_SUM_SINGLECPU_RL
C      o GLOBAL_SUM_SINGLECPU_RS <- not yet coded

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_SINGLECPU_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_SINGLECPU_RL(
     I                       phiLocal,
     O                       sumPhi,
     I                       oLi, oLj, myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_SINGLECPU\_RL
C     | o Handle sum for _RL data.
C     *==========================================================*
C     | Global sum of 2d array
C     | independent of tiling as sum is performed on a single CPU
C     | sum is performed in REAL*8
C     *==========================================================*

C     !USES:
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "GLOBAL_SUM.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif
#include "EEBUFF_SCPU.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     phiLocal :: local input array without overlap regions.
C     sumPhi   :: Result of sum.
C     oLi, oLj :: overlap size of input array in I & J direction.
C     myThid   :: My thread id.
      INTEGER oLi, oLj
      _RL    phiLocal(1-oLi:sNx+oLi,1-oLj:sNy+oLj,nSx,nSy)
      _RL     sumPhi
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C- type declaration of: sumAll, globalBuf :
C         sumAll needs to have the same length as MPI_DOUBLE_PRECISION
      LOGICAL useExch2GlobLayOut, zeroBuff
      INTEGER xSize, ySize
      INTEGER i, j, ij
      INTEGER bi, bj
      Real*8  sumAll
#ifdef ALLOW_USE_MPI
      INTEGER pId, idest, itag
      INTEGER istatus(MPI_STATUS_SIZE), ierr
#endif /* ALLOW_USE_MPI */
CEOP

#ifdef ALLOW_EXCH2
      zeroBuff = .TRUE.
      useExch2GlobLayOut = .TRUE.
      xSize = exch2_global_Nx
      ySize = exch2_global_Ny
#else /* ALLOW_EXCH2 */
      zeroBuff = .FALSE.
      useExch2GlobLayOut = .FALSE.
      xSize = Nx
      ySize = Ny
#endif /* ALLOW_EXCH2 */
#ifdef ALLOW_USE_MPI
      idest = 0
      itag  = 0
#endif

C--   copy (and conversion to real*8) to Shared buffer:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           sharedLocBuf_r8(i,j,bi,bj) = phiLocal(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Master thread does the communications and the global sum
C--   Master thread cannot start until everyone is ready:
      CALL BAR2( myThid )
      _BEGIN_MASTER( myThid )

C--   Gather local arrays
      CALL GATHER_2D_R8(
     O                       xy_buffer_r8,
     I                       sharedLocBuf_r8,
     I                       xSize, ySize,
     I                       useExch2GlobLayOut, zeroBuff, myThid )

      IF ( myProcId.EQ.0 ) THEN

C--   Process 0 sums the global array
        sumAll = 0. _d 0
        DO ij=1,xSize*ySize
          sumAll = sumAll + xy_buffer_r8(ij)
        ENDDO

#ifdef ALLOW_USE_MPI
C--   Process 0 sends result to all other processes
        IF ( usingMPI ) THEN
         DO pId = 1, (nPx*nPy)-1
          CALL MPI_SEND (sumAll, 1, MPI_DOUBLE_PRECISION,
     &                   pId, itag, MPI_COMM_MODEL, ierr)
         ENDDO
        ENDIF

      ELSEIF ( usingMPI ) THEN

C--   All proceses except 0 receive result from process 0
          CALL MPI_RECV (sumAll, 1, MPI_DOUBLE_PRECISION,
     &         idest, itag, MPI_COMM_MODEL, istatus, ierr)

#endif /* ALLOW_USE_MPI */

      ENDIF

C--   Write solution to shared buffer (all threads can see it)
      phiGSR8(1,0) = sumAll

      _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
      CALL BAR2( myThid )

C--   set result for every threads
      sumPhi = phiGSR8(1,0)

      RETURN
      END
