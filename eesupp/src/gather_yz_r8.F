#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

CBOP
C !ROUTINE: GATHER_YZ_R8
C !INTERFACE:
      SUBROUTINE GATHER_YZ_R8(
     O                  gloBuff,
     I                  myField,
     I                  ySize,
     I                  useExch2GlobLayOut,
     I                  zeroBuff,
     I                  myThid )
C !DESCRIPTION:
C     Gather elements of a global 2-D array from all mpi processes to process 0.
C     Note: done by Master-Thread ; might need barrier calls before and after
C           this S/R call.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C gloBuff   ( _R8 ) :: full-domain 2D IO-buffer array             (Output)
C myField   ( _R8 ) :: tiled, local (i.e. my Proc. tiles) 2D array (Input)
C ySize    (integer):: global buffer 1rst dim (y)
C useExch2GlobLayOut:: =T: Use Exch2 global-map layout (only with EXCH2)
C zeroBuff (logical):: =T: initialise the buffer to zero before copy
C myThid   (integer):: my Thread Id number

      INTEGER ySize
      _R8     gloBuff(ySize)
      _R8     myField(1:sNy,nSx,nSy)
      LOGICAL useExch2GlobLayOut
      LOGICAL zeroBuff
      INTEGER myThid
CEOP

C !LOCAL VARIABLES:
      INTEGER j,bi,bj
      INTEGER jG
      INTEGER jBase
#ifdef ALLOW_EXCH2
      INTEGER jGjLoc
      INTEGER tN
#endif /* ALLOW_EXCH2 */
#ifdef ALLOW_USE_MPI
      INTEGER np, pId
      _R8     temp(1:sNy,nSx,nSy)
      INTEGER istatus(MPI_STATUS_SIZE), ierr
      INTEGER lbuff, idest, itag, ready_to_receive
#endif /* ALLOW_USE_MPI */

      _BEGIN_MASTER( myThid )

      IF( myProcId .EQ. 0 ) THEN
C--   Process 0 fills-in its local data

#ifdef ALLOW_EXCH2
        IF ( useExch2GlobLayOut ) THEN
C--   If using blank-tiles, buffer will not be completely filled;
C     safer to reset to zero to avoid unknown values in output file
          IF ( zeroBuff ) THEN
            DO j=1,ySize
               gloBuff(j) = 0.
            ENDDO
          ENDIF

          DO bj=1,nSy
           DO bi=1,nSx
            tN = W2_myTileList(bi,bj)
            IF  ( exch2_tNy(tN) .GT. ySize ) THEN
C-          tile y-size larger than glob-size : make a long line
             jGjLoc = 0
            ELSE
C-          default (face fit into global-IO-array)
             jGjLoc = 1
            ENDIF

            DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
             jG=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
             IF (myField(j,bi,bj).NE.UNSET_FLOAT8)
     &        gloBuff(jG) = myField(j,bi,bj)
            ENDDO
           ENDDO
          ENDDO

        ELSE
#else /* ALLOW_EXCH2 */
        IF (.TRUE.) THEN
#endif /* ALLOW_EXCH2 */

          jBase = myYGlobalLo-1

          DO bj=1,nSy
           DO bi=1,nSx
            jG = jBase+(bj-1)*sNy
            DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
             IF (myField(j,bi,bj).NE.UNSET_FLOAT8)
     &        gloBuff(jG+j) = myField(j,bi,bj)
            ENDDO
           ENDDO
          ENDDO

C       end if-else useExch2GlobLayOut
        ENDIF

C-    end if myProcId = 0
      ENDIF

#ifdef ALLOW_USE_MPI
      IF ( usingMPI ) THEN

       lbuff = sNy*nSx*nSy
       idest = 0
       itag  = 0
       ready_to_receive = 0

       IF( mpiMyId .EQ. 0 ) THEN

C--   Process 0 polls and receives data from each process in turn
        DO np = 2, nPx*nPy
         pId = np - 1
#ifndef DISABLE_MPI_READY_TO_RECEIVE
         CALL MPI_SEND (ready_to_receive, 1, MPI_INTEGER,
     &           pId, itag, MPI_COMM_MODEL, ierr)
#endif
         CALL MPI_RECV (temp, lbuff, _MPI_TYPE_R8,
     &           pId, itag, MPI_COMM_MODEL, istatus, ierr)

C--   Process 0 gathers the local arrays into the global buffer.
#ifdef ALLOW_EXCH2
         IF ( useExch2GlobLayOut ) THEN

          DO bj=1,nSy
           DO bi=1,nSx
            tN = W2_procTileList(bi,bj,np)
            IF  ( exch2_tNy(tN) .GT. ySize ) THEN
C-          tile y-size larger than glob-size : make a long line
             jGjLoc = 0
            ELSE
C-          default (face fit into global-IO-array)
             jGjLoc = 1
            ENDIF

            DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
             jG=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
             IF (temp(j,bi,bj).NE.UNSET_FLOAT8)
     &        gloBuff(jG) = temp(j,bi,bj)
            ENDDO
           ENDDO
          ENDDO

         ELSE
#else /* ALLOW_EXCH2 */
         IF (.TRUE.) THEN
#endif /* ALLOW_EXCH2 */

          jBase = mpi_myYGlobalLo(np)-1

          DO bj=1,nSy
           DO bi=1,nSx
            jG = jBase+(bj-1)*sNy
            DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
             IF (temp(j,bi,bj).NE.UNSET_FLOAT8)
     &        gloBuff(jG+j) = temp(j,bi,bj)
            ENDDO
           ENDDO
          ENDDO

C        end if-else useExch2GlobLayOut
         ENDIF

C-      end loop on np
        ENDDO

       ELSE

C--   All proceses except 0 wait to be polled then send local array
#ifndef DISABLE_MPI_READY_TO_RECEIVE
         CALL MPI_RECV (ready_to_receive, 1, MPI_INTEGER,
     &        idest, itag, MPI_COMM_MODEL, istatus, ierr)
#endif
         CALL MPI_SEND (myField, lbuff, _MPI_TYPE_R8,
     &        idest, itag, MPI_COMM_MODEL, ierr)

       ENDIF

      ENDIF
#endif /* ALLOW_USE_MPI */

      _END_MASTER( myThid )

      RETURN
      END
