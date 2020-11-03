#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

CBOP
C !ROUTINE: SCATTER_YZ_R8
C !INTERFACE:
      SUBROUTINE SCATTER_YZ_R8(
     I                  gloBuff,
     O                  myField,
     I                  ySize, slice,
     I                  useExch2GlobLayOut,
     I                  zeroBuff,
     I                  myThid )
C !DESCRIPTION:
C     Scatter elements of a global 2-D array from mpi process 0 to all processes.
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
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C gloBuff   ( _R8 ) :: full-domain 2D IO-buffer array              (Input)
C myField   ( _R8 ) :: tiled, local (i.e. my Proc. tiles) 2D array (Output)
C ySize    (integer):: global buffer 2nd  dim (y)
C slice    (char(5)):: denoting east or west slice
C useExch2GlobLayOut:: =T: Use Exch2 global-map layout (only with EXCH2)
C zeroBuff (logical):: =T: reset the buffer to zero after copy
C myThid   (integer):: my Thread Id number

      INTEGER ySize
      _R8     gloBuff(ySize)
      _R8     myField(1:sNy,nSx,nSy)
      CHARACTER*(5) slice
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
      INTEGER lbuff, isource, itag
#endif /* ALLOW_USE_MPI */

#ifdef ALLOW_OBCS

      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
      IF ( usingMPI ) THEN

       lbuff = sNy*nSx*nSy
       isource = 0
       itag  = 0

       IF( mpiMyId .EQ. 0 ) THEN

C--   Process 0 sends local arrays to all other processes
        DO np = 2, nPx*nPy

C--   Process 0 extract the local arrays from the global buffer.

#ifdef ALLOW_EXCH2
         IF ( useExch2GlobLayOut ) THEN

          DO bj=1,nSy
           DO bi=1,nSx
            IF (((slice(1:4).EQ.'east').AND.(tileHasOBE(bi,bj))).OR.
     &          ((slice(1:4).EQ.'west').AND.(tileHasOBW(bi,bj)))) THEN
             tN = W2_procTileList(bi,bj,np)
             IF ( exch2_tNy(tN) .GT. ySize ) THEN
C-           tile y-size larger than glob-size : make a long line
              jGjLoc = 0
             ELSE
C-           default (face fit into global-IO-array)
              jGjLoc = 1
             ENDIF

             DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
              jG=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
              temp(j,bi,bj) = gloBuff(jG)
             ENDDO
            ENDIF
           ENDDO
          ENDDO

         ELSE
#else /* ALLOW_EXCH2 */
         IF (.TRUE.) THEN
#endif /* ALLOW_EXCH2 */

          jBase = mpi_myYGlobalLo(np)-1

          DO bj=1,nSy
           DO bi=1,nSx
            IF (((slice(1:4).EQ.'east').AND.(tileHasOBE(bi,bj))).OR.
     &          ((slice(1:4).EQ.'west').AND.(tileHasOBW(bi,bj)))) THEN
             jG = jBase+(bj-1)*sNy
             DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
              temp(j,bi,bj) = gloBuff(jG+j)
             ENDDO
            ENDIF
           ENDDO
          ENDDO

C        end if-else useExch2GlobLayOut
         ENDIF

C--   Process 0 sends local arrays to all other processes
         pId = np - 1
         CALL MPI_SEND (temp, lbuff, _MPI_TYPE_R8,
     &           pId, itag, MPI_COMM_MODEL, ierr)

C-      end loop on np
        ENDDO

       ELSE

C--   All proceses except 0 receive local array from process 0
         CALL MPI_RECV (myField, lbuff, _MPI_TYPE_R8,
     &        isource, itag, MPI_COMM_MODEL, istatus, ierr)

       ENDIF

      ENDIF
#endif /* ALLOW_USE_MPI */

      IF( myProcId .EQ. 0 ) THEN
C--   Process 0 fills-in its local data

#ifdef ALLOW_EXCH2
        IF ( useExch2GlobLayOut ) THEN

          DO bj=1,nSy
           DO bi=1,nSx
            IF (((slice(1:4).EQ.'east').AND.(tileHasOBE(bi,bj))).OR.
     &          ((slice(1:4).EQ.'west').AND.(tileHasOBW(bi,bj)))) THEN
             tN = W2_myTileList(bi,bj)
             IF ( exch2_tNy(tN) .GT. ySize ) THEN
C-           tile y-size larger than glob-size : make a long line
              jGjLoc = 0
             ELSE
C-           default (face fit into global-IO-array)
              jGjLoc = 1
             ENDIF

             DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
              jG=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
              myField(j,bi,bj) = gloBuff(jG)
             ENDDO
            ENDIF
           ENDDO
          ENDDO

C--   After the copy from the buffer, reset to zero.
C     An alternative to zeroBuff when writing to file,
C     which could be faster if we do less read than write.
          IF ( zeroBuff ) THEN
            DO j=1,ySize
              gloBuff(j) = 0.
            ENDDO
          ENDIF

        ELSE
#else /* ALLOW_EXCH2 */
        IF (.TRUE.) THEN
#endif /* ALLOW_EXCH2 */

          jBase = myYGlobalLo-1

          DO bj=1,nSy
           DO bi=1,nSx
            IF (((slice(1:4).EQ.'east').AND.(tileHasOBE(bi,bj))).OR.
     &          ((slice(1:4).EQ.'west').AND.(tileHasOBW(bi,bj)))) THEN
             jG = jBase+(bj-1)*sNy
             DO j=1,sNy
#ifdef TARGET_NEC_SX
!cdir novector
#endif
              myField(j,bi,bj) = gloBuff(jG+j)
             ENDDO
            ENDIF
           ENDDO
          ENDDO

C       end if-else useExch2GlobLayOut
        ENDIF

C-    end if myProcId = 0
      ENDIF

      _END_MASTER( myThid )

#endif

      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
