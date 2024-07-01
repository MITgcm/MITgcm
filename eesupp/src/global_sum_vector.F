#include "CPP_EEOPTIONS.h"

C--   File global_sum_vector.F: Routines that perform global sum
C                               on a tile array, vector field
C--    Contents
C--    o GLB_SUM_VEC           - Global-Sum vector from Master thread
C--    o GLOBAL_SUM_VECTOR_RL  - Global-Sum vector (input: nSx,nSy,nVec)
C--    o GLOBAL_SUM_VECTOR_RS  - Global-Sum vector (input: nSx,nSy,nVec)
C--    o GLOBAL_SUM_VEC_ALT_RL - Global-Sum vector (input: nDim,nSx,nSy)
C--    o GLOBAL_SUM_VEC_ALT_RS - Global-Sum vector (input: nDim,nSx,nSy)
C--    o GLOBAL_SUM_VECTOR_INT - Global-Sum vector (input: nSx,nSy,nVec)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLB_SUM_VEC

C     !INTERFACE:
      SUBROUTINE GLB_SUM_VEC(
     I                        nVec, tiledVec,
     O                        sumVec,
     I                        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLB\_SUM\_VEC
C     | o Handle global sum from master thread
C     *==========================================================*
C     | Apply sum on a shared array of tiled vector
C     |  and operate over all tiles & all the processes.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EEBUFF_SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     nVec     :: length of input/output vector
C     tiledVec :: Input tiled vector (dims: nSx,nSy,nVec)
C     sumVec   :: Result of sum over all tiles & procs
C     myThid   :: My thread id.
      INTEGER nVec
      Real*8  tiledVec(nSx,nSy,nVec)
      Real*8  sumVec(nVec)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     n, bi,bj :: Loop counters
C     mpiRC    :: MPI return code
C     msgBuf   :: Informational/error message buffer
C- type declaration of: sumMyPr, sumVec, localSum and globalSum
C          all 4 needs to have the same length as MPI_DOUBLE_PRECISION
      INTEGER n, bi, bj
#ifdef ALLOW_USE_MPI
      INTEGER mpiRC
# ifdef GLOBAL_SUM_ORDER_TILES
      INTEGER biG, bjG, fullLength
      Real*8  localSum (nSx*nPx,nSy*nPy,GSVec_size)
      Real*8  globalSum(nSx*nPx,nSy*nPy,GSVec_size)
# endif /* GLOBAL_SUM_ORDER_TILES */
#endif /* ALLOW_USE_MPI */
      Real*8  sumMyPr(GSVec_size)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( nVec.GT.GSVec_size ) THEN
        WRITE(msgBuf,'(A,2(A,I7))') 'GLB_SUM_VEC: ',
     &       'nVec=', nVec, ' > GSVec_size=', GSVec_size
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'GLB_SUM_VEC: ',
     &       'increase "GSVec_size" from EEBUFF_SIZE.h and recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GLB_SUM_VEC'
      ENDIF

      _BEGIN_MASTER( myThid )

#if ( defined GLOBAL_SUM_ORDER_TILES && defined ALLOW_USE_MPI )
      IF ( usingMPI ) THEN

C--   Initialise local buffer
       DO n = 1,nVec
        DO bjG=1,nSy*nPy
         DO biG=1,nSx*nPx
          localSum(biG,bjG,n) = 0.
         ENDDO
        ENDDO
       ENDDO

C--   Put my own data in local buffer
       DO bj=1,nSy
        bjG = (myYGlobalLo-1)/sNy+bj
        DO bi=1,nSx
         biG = (myXGlobalLo-1)/sNx+bi
         DO n = 1,nVec
          localSum(biG,bjG,n) = tiledVec(bi,bj,n)
         ENDDO
        ENDDO
       ENDDO

C--   Collect data from all procs
       fullLength = nSx*nPx*nSy*nPy*nVec
       CALL MPI_Allreduce( localSum, globalSum, fullLength,
     &          MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_MODEL, mpiRC )

C--   Sum over all tiles:
       DO n = 1,nVec
        sumVec(n) = 0.
       ENDDO
       DO bjG = 1,nSy*nPy
        DO biG = 1,nSx*nPx
         DO n = 1,nVec
          sumVec(n) = sumVec(n) + globalSum(biG,bjG,n)
         ENDDO
        ENDDO
       ENDDO

      ELSE
#else /* not (GLOBAL_SUM_ORDER_TILES & ALLOW_USE_MPI) */
      IF ( .TRUE. ) THEN
#endif /* not (GLOBAL_SUM_ORDER_TILES & ALLOW_USE_MPI) */

C--   Sum over all tiles (of the same process) first
       DO n = 1,nVec
        sumMyPr(n) = 0.
       ENDDO
       DO bj = 1,nSy
        DO bi = 1,nSx
         DO n = 1,nVec
          sumMyPr(n) = sumMyPr(n) + tiledVec(bi,bj,n)
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_USE_MPI
       IF ( usingMPI ) THEN
C--   sum over all procs and put result into shared buffer:
        CALL MPI_Allreduce( sumMyPr, sumVec, nVec,
     &           MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_MODEL, mpiRC )
       ELSE
#else /* ALLOW_USE_MPI */
       IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
C--   in case MPI is not used, put result directly in shared buffer:
        DO n = 1,nVec
         sumVec(n) = sumMyPr(n)
        ENDDO
       ENDIF

      ENDIF

      _END_MASTER( myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_VECTOR_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_VECTOR_RL(
     I                       nVec, tiledVec,
     O                       sumVec,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_VECTOR\_RL
C     | o Handle sum for _RL vector.
C     *==========================================================*
C     | Apply sum on an array of tiled vector (input: nSx,nSy,nVec)
C     |  and operate over all tiles & all the processes.
C     | Oversize vector is cut and processed in "nCuts" pieces
C     |  that fit in buffer
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EEBUFF_SIZE.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     nVec     :: length of input/output vector
C     tiledVec :: Input tiled vector (dims: nSx,nSy,nVec)
C     sumVec   :: Result of sum over all tiles, threads & procs
C     myThid   :: My thread id.
      INTEGER nVec
      _RL     tiledVec(nSx,nSy,nVec)
      _RL     sumVec(nVec)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     n, j, j0 :: Loop counters
C     nCuts    :: range of outside loop
C     nLoc     :: range of inner loop
      INTEGER bi, bj
      INTEGER n, nCuts
      INTEGER j, j0, nLoc
CEOP

C--   starts outside loop over number of "cuts", each fitting into buffer size
      nCuts = 1 + INT( (nVec-1)/GSVec_size )
      DO n = 1,nCuts
       j0 = (n-1)*GSVec_size
       nLoc = MIN( n*GSVec_size, nVec ) - j0

C--   write local sum into shared-buffer array
       DO j = 1,nLoc
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shareBufGSVec(bi,bj,j) = tiledVec(bi,bj,j+j0)
         ENDDO
        ENDDO
       ENDDO

C--   Master thread cannot start until everyone is ready:
       CALL BAR2( myThid )
       _BEGIN_MASTER( myThid )

C--   From master-thread, do the global-sum on shared-buffer
       CALL GLB_SUM_VEC(
     I                   nLoc, shareBufGSVec,
     O                   shareGSVector,
     I                   myThid )

       _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
       CALL BAR2( myThid )

C--   set result for every threads
       DO j=1,nLoc
        sumVec(j+j0) = shareGSVector(j)
       ENDDO

C--   end outside do-loop over nCuts
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_VECTOR_RS

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_VECTOR_RS(
     I                       nVec, tiledVec,
     O                       sumVec,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_VECTOR\_RS
C     | o Handle sum for _RS vector.
C     *==========================================================*
C     | Apply sum on an array of tiled vector (input: nSx,nSy,nVec)
C     |  and operate over all tiles & all the processes.
C     | Oversize vector is cut and processed in "nCuts" pieces
C     |  that fit in buffer
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EEBUFF_SIZE.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     nVec     :: length of input/output vector
C     tiledVec :: Input tiled vector (dims: nSx,nSy,nVec)
C     sumVec   :: Result of sum over all tiles, threads & procs
C     myThid   :: My thread id.
      INTEGER nVec
      _RS     tiledVec(nSx,nSy,nVec)
      _RS     sumVec(nVec)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     n, j, j0 :: Loop counters
C     nCuts    :: range of outside loop
C     nLoc     :: range of inner loop
      INTEGER bi, bj
      INTEGER n, nCuts
      INTEGER j, j0, nLoc
CEOP

C--   starts outside loop over number of "cuts", each fitting into buffer size
      nCuts = 1 + INT( (nVec-1)/GSVec_size )
      DO n = 1,nCuts
       j0 = (n-1)*GSVec_size
       nLoc = MIN( n*GSVec_size, nVec ) - j0

C--   write local sum into shared-buffer array (+ convert _RS to r8)
       DO j = 1,nLoc
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shareBufGSVec(bi,bj,j) = tiledVec(bi,bj,j+j0)
         ENDDO
        ENDDO
       ENDDO

C--   Master thread cannot start until everyone is ready:
       CALL BAR2( myThid )
       _BEGIN_MASTER( myThid )

C--   From master-thread, do the global-sum on shared-buffer
       CALL GLB_SUM_VEC(
     I                   nLoc, shareBufGSVec,
     O                   shareGSVector,
     I                   myThid )

       _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
       CALL BAR2( myThid )

C--   set result for every threads (+ convert back r8 to _RS)
       DO j=1,nLoc
        sumVec(j+j0) = shareGSVector(j)
       ENDDO

C--   end outside do-loop over nCuts
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_VEC_ALT_RL

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_VEC_ALT_RL(
     I                       nDim, nVec, vecTile,
     O                       vecSum,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_VEC\_ALT\_RL
C     | o Handle sum for _RL vector.
C     *==========================================================*
C     | Apply sum on an array of tiled vector (input: nDim,nSx,nSy)
C     |  and operate over all tiles & all the processes.
C     | Oversize vector is cut and processed in "nCuts" pieces
C     |  that fit in buffer
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EEBUFF_SIZE.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     nDim     :: dimension of input vector
C     nVec     :: length of input/output vector to process
C     vecTile  :: Input tiled vector (dims: nDim,nSx,nSy)
C     vecSum   :: Result of sum over all tiles, threads & procs
C     myThid   :: My thread id.
      INTEGER nDim, nVec
      _RL     vecTile(nDim,nSx,nSy)
      _RL     vecSum(nVec)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     n, j, j0 :: Loop counters
C     nCuts    :: range of outside loop
C     nLoc     :: range of inner loop
C     msgBuf   :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER n, nCuts
      INTEGER j, j0, nLoc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( nVec.GT.nDim ) THEN
       WRITE(msgBuf,'(A,2(A,I7))') 'GLOBAL_SUM_VEC_ALT_RL: ',
     &      'nVec=', nVec, ' > nDim =', nDim
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GLOBAL_SUM_VEC_ALT_RL'
      ENDIF

C--   starts outside loop over number of "cuts", each fitting into buffer size
      nCuts = 1 + INT( (nVec-1)/GSVec_size )
      DO n = 1,nCuts
       j0 = (n-1)*GSVec_size
       nLoc = MIN( n*GSVec_size, nVec ) - j0

C--   write local sum into shared-buffer array
       DO j = 1,nLoc
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shareBufGSVec(bi,bj,j) = vecTile(j+j0,bi,bj)
         ENDDO
        ENDDO
       ENDDO

C--   Master thread cannot start until everyone is ready:
       CALL BAR2( myThid )
       _BEGIN_MASTER( myThid )

C--   From master-thread, do the global-sum on shared-buffer
       CALL GLB_SUM_VEC(
     I                   nLoc, shareBufGSVec,
     O                   shareGSVector,
     I                   myThid )

       _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
       CALL BAR2( myThid )

C--   set result for every threads
       DO j=1,nLoc
        vecSum(j+j0) = shareGSVector(j)
       ENDDO

C--   end outside do-loop over nCuts
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_VEC_ALT_RS

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_VEC_ALT_RS(
     I                       nDim, nVec, vecTile,
     O                       vecSum,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_VEC\_ALT\_RS
C     | o Handle sum for _RS vector.
C     *==========================================================*
C     | Apply sum on an array of tiled vector (input: nDim,nSx,nSy)
C     |  and operate over all tiles & all the processes.
C     | Oversize vector is cut and processed in "nCuts" pieces
C     |  that fit in buffer
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EEBUFF_SIZE.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     nDim     :: dimension of input vector
C     nVec     :: length of input/output vector to process
C     vecTile  :: Input tiled vector (dims: nDim,nSx,nSy)
C     vecSum   :: Result of sum over all tiles, threads & procs
C     myThid   :: My thread id.
      INTEGER nDim, nVec
      _RS     vecTile(nDim,nSx,nSy)
      _RS     vecSum(nVec)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     n, j, j0 :: Loop counters
C     nCuts    :: range of outside loop
C     nLoc     :: range of inner loop
C     msgBuf   :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER n, nCuts
      INTEGER j, j0, nLoc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( nVec.GT.nDim ) THEN
       WRITE(msgBuf,'(A,2(A,I7))') 'GLOBAL_SUM_VEC_ALT_RS: ',
     &      'nVec=', nVec, ' > nDim =', nDim
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GLOBAL_SUM_VEC_ALT_RS'
      ENDIF

C--   starts outside loop over number of "cuts", each fitting into buffer size
      nCuts = 1 + INT( (nVec-1)/GSVec_size )
      DO n = 1,nCuts
       j0 = (n-1)*GSVec_size
       nLoc = MIN( n*GSVec_size, nVec ) - j0

C--   write local sum into shared-buffer array (+ convert _RS to r8)
       DO j = 1,nLoc
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shareBufGSVec(bi,bj,j) = vecTile(j+j0,bi,bj)
         ENDDO
        ENDDO
       ENDDO

C--   Master thread cannot start until everyone is ready:
       CALL BAR2( myThid )
       _BEGIN_MASTER( myThid )

C--   From master-thread, do the global-sum on shared-buffer
       CALL GLB_SUM_VEC(
     I                   nLoc, shareBufGSVec,
     O                   shareGSVector,
     I                   myThid )

       _END_MASTER( myThid )
C--   Everyone wait for Master thread to be ready
       CALL BAR2( myThid )

C--   set result for every threads (+ convert back r8 to _RS)
       DO j=1,nLoc
        vecSum(j+j0) = shareGSVector(j)
       ENDDO

C--   end outside do-loop over nCuts
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_SUM_VECTOR_INT

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_VECTOR_INT(
     I                       nVec, tiledVec,
     O                       sumVec,
     I                       myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL\_SUM\_VECTOR\_INT
C     | o Handle sum for Integer vector.
C     *==========================================================*
C     | Apply sum on an array of tiled vector (input: nSx,nSy,nVec)
C     |  and operate over all tiles & all the processes.
C     | Oversize vector is cut and processed in "nCuts" pieces
C     |  that fit in buffer
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EEBUFF_SIZE.h"
#include "GLOBAL_SUM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     nVec     :: length of input/output vector
C     tiledVec :: Input tiled vector (dims: nSx,nSy,nVec)
C     sumVec   :: Result of sum over all tiles, threads & procs
C     myThid   :: My thread id.
      INTEGER nVec
      INTEGER tiledVec(nSx,nSy,nVec)
      INTEGER sumVec(nVec)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj   :: tile indices
C     n, j, j0 :: Loop counters
C     nCuts    :: range of outside loop
C     nLoc     :: range of inner loop
C     mpiRC    :: MPI return code
      INTEGER bi, bj
      INTEGER n, nCuts
      INTEGER j, j0, nLoc
      INTEGER sumMyPr(GSVec_size)
C- type declaration of: sumMyPr & shareGSVectInt need to have
C                       the same length as MPI_INTEGER
#ifdef ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
CEOP

C--   starts outside loop over number of "cuts", each fitting into buffer size
      nCuts = 1 + INT( (nVec-1)/GSVec_size )
      DO n = 1,nCuts
       j0 = (n-1)*GSVec_size
       nLoc = MIN( n*GSVec_size, nVec ) - j0

C--   write local sum into shared-buffer array
       DO j = 1,nLoc
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shareBufGSVecI(bi,bj,j) = tiledVec(bi,bj,j+j0)
         ENDDO
        ENDDO
       ENDDO

C--   Master thread cannot start until everyone is ready:
       CALL BAR2( myThid )
       _BEGIN_MASTER( myThid )

C--   Sum over all tiles (of the same process) first
        DO j = 1,nLoc
         sumMyPr(n) = 0
        ENDDO
        DO bj = 1,nSy
         DO bi = 1,nSx
          DO j = 1,nLoc
           sumMyPr(j) = sumMyPr(j) + shareBufGSVecI(bi,bj,j)
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_USE_MPI
        IF ( usingMPI ) THEN
C--   sum over all procs and put result into shared buffer:
         CALL MPI_Allreduce( sumMyPr, shareGSVectInt, nLoc,
     &            MPI_INTEGER, MPI_SUM, MPI_COMM_MODEL, mpiRC )
        ELSE
#else /* ALLOW_USE_MPI */
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
C--   in case MPI is not used, put result directly in shared buffer:
         DO j = 1,nLoc
          shareGSVectInt(j) = sumMyPr(j)
         ENDDO
        ENDIF

       _END_MASTER( myThid )

C--   Everyone wait for Master thread to be ready
       CALL BAR2( myThid )

C--   set result for every threads
       DO j = 1,nLoc
        sumVec(j+j0) = shareGSVectInt(j)
       ENDDO

C--   end outside do-loop over nCuts
      ENDDO

      RETURN
      END
