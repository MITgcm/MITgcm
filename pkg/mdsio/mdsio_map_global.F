C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_map_global.F,v 1.1 2009/05/06 02:42:49 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_MAP_GLOBAL
C !INTERFACE:
      SUBROUTINE MDS_MAP_GLOBAL(
     U   buff_r4,
     U   buff_r8,
     U   array,
     I   xSize, ySize, buffPrec,
     I   copyTo, zeroBuff )

C !DESCRIPTION:
C MDS_MAP_GLOBAL  map global io-buffer to global model 2-D Real*8 array
C                 or the reverse, depending on "copyTo" value
C
C Arguments:
C
C buff_r4  (real*4 ):: full-domain 2D IO-buffer array (Inp./Outp. if copyTo=T/F)
C buff_r8  (real*8 ):: full-domain 2D IO-buffer array (Inp./Outp. if copyTo=T/F)
C array    (real*8 ):: model 2D global array          (Outp./Inp. if copyTo=T/F)
C xSize    (integer):: buffers 1rst dim (x)
C ySize    (integer):: buffers 2nd  dim (y)
C buffPrec (integer):: precision (32 or 64) of which buffer to copy (from/to)
C copyTo   (logical):: =T: copy IO-buffer to array ; =F: copy array to IO-buffer
C zeroBuff (logical):: =T: initialise the buffer to zero after/before copy
C
CEOP

C !USES:
      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C !INPUT/OUTPUT PARAMETERS:
      INTEGER xSize, ySize
      Real*4  buff_r4 ( xSize, ySize )
      Real*8  buff_r8 ( xSize, ySize )
      Real*8  array   ( Nx, Ny )
      INTEGER buffPrec
      LOGICAL copyTo
      LOGICAL zeroBuff

C !LOCAL VARIABLES:
      INTEGER i,j,bi,bj
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      INTEGER iG, jG
      INTEGER iG_IO, jG_IO, npe, loc_xGlobalLo, loc_yGlobalLo
      INTEGER iGjLoc, jGjLoc
      INTEGER tN
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
C--   If using blank-tiles, buffer will not be completely filled;
C     safer to reset to zero to avoid unknown values in output file
        IF ( zeroBuff .AND. .NOT.copyTo ) THEN
          IF ( buffPrec.EQ.precFloat32 ) THEN
            DO j=1,ySize
             DO i=1,xSize
               buff_r4(I,J) = 0.
             ENDDO
            ENDDO
          ELSEIF ( buffPrec.EQ.precFloat64 ) THEN
            DO j=1,ySize
             DO i=1,xSize
               buff_r8(I,J) = 0. _d 0
            ENDDO
           ENDDO
          ENDIF
        ENDIF

        bj=1
        DO npe=1,nPx*nPy
#ifdef ALLOW_USE_MPI
          loc_xGlobalLo = mpi_myXGlobalLo(npe)
          loc_yGlobalLo = mpi_myYGlobalLo(npe)
#else  /* ALLOW_USE_MPI */
          loc_xGlobalLo = myXGlobalLo
          loc_yGlobalLo = myYGlobalLo
#endif /* ALLOW_USE_MPI */
         DO bi=1,nSx
          tN = W2_mpi_myTileList(npe,bi)
          IF   ( exch2_mydNx(tN) .GT. xSize ) THEN
C-        face x-size larger than glob-size : fold it
            iGjLoc = 0
            jGjLoc = exch2_mydNx(tN) / xSize
          ELSEIF ( exch2_tNy(tN) .GT. ySize ) THEN
C-        tile y-size larger than glob-size : make a long line
            iGjLoc = exch2_mydNx(tN)
            jGjLoc = 0
          ELSE
C-        default (face fit into global-IO-array)
            iGjLoc = 0
            jGjLoc = 1
          ENDIF

          IF ( buffPrec.EQ.precFloat32 ) THEN
           IF ( copyTo ) THEN
             DO j=1,sNy
              DO i=1,sNx
               iG = loc_xGlobalLo-1+(bi-1)*sNx+i
               jG = loc_yGlobalLo-1+(bj-1)*sNy+j
               iG_IO=exch2_txGlobalo(tN)+iGjLoc*(j-1)+i-1
               jG_IO=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
               array(iG,jG) = buff_r4(iG_IO,jG_IO)
              ENDDO
             ENDDO
           ELSE
             DO j=1,sNy
              DO i=1,sNx
               iG = loc_xGlobalLo-1+(bi-1)*sNx+i
               jG = loc_yGlobalLo-1+(bj-1)*sNy+j
               iG_IO=exch2_txGlobalo(tN)+iGjLoc*(j-1)+i-1
               jG_IO=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
               buff_r4(iG_IO,jG_IO) = array(iG,jG)
              ENDDO
             ENDDO
           ENDIF
          ELSEIF ( buffPrec.EQ.precFloat64 ) THEN
           IF ( copyTo ) THEN
             DO j=1,sNy
              DO i=1,sNx
               iG = loc_xGlobalLo-1+(bi-1)*sNx+i
               jG = loc_yGlobalLo-1+(bj-1)*sNy+j
               iG_IO=exch2_txGlobalo(tN)+iGjLoc*(j-1)+i-1
               jG_IO=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
               array(iG,jG) = buff_r8(iG_IO,jG_IO)
              ENDDO
             ENDDO
           ELSE
             DO j=1,sNy
              DO i=1,sNx
               iG = loc_xGlobalLo-1+(bi-1)*sNx+i
               jG = loc_yGlobalLo-1+(bj-1)*sNy+j
               iG_IO=exch2_txGlobalo(tN)+iGjLoc*(j-1)+i-1
               jG_IO=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
               buff_r8(iG_IO,jG_IO) = array(iG,jG)
              ENDDO
             ENDDO
           ENDIF
          ENDIF

C--    end of npe & bi loops
         ENDDO
        ENDDO

C--   After the copy from the buffer, reset to zero.
C     An alternative to zeroBuff when writing to file,
C     which could be faster if we do less read than write.
        IF ( zeroBuff .AND. copyTo ) THEN
          IF ( buffPrec.EQ.precFloat32 ) THEN
            DO j=1,ySize
             DO i=1,xSize
               buff_r4(I,J) = 0.
             ENDDO
            ENDDO
          ELSEIF ( buffPrec.EQ.precFloat64 ) THEN
            DO j=1,ySize
             DO i=1,xSize
               buff_r8(I,J) = 0. _d 0
            ENDDO
           ENDDO
          ENDIF
        ENDIF

#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
        IF ( buffPrec.EQ.precFloat32 ) THEN
         IF ( copyTo ) THEN
           DO j=1,Ny
            DO i=1,Nx
             array(i,j) = buff_r4(i,j)
            ENDDO
           ENDDO
         ELSE
           DO j=1,Ny
            DO i=1,Nx
             buff_r4(i,j) = array(i,j)
            ENDDO
           ENDDO
         ENDIF
        ELSEIF ( buffPrec.EQ.precFloat64 ) THEN
         IF ( copyTo ) THEN
           DO j=1,Ny
            DO i=1,Nx
             array(i,j) = buff_r8(i,j)
            ENDDO
           ENDDO
         ELSE
           DO j=1,Ny
            DO i=1,Nx
             buff_r8(i,j) = array(i,j)
            ENDDO
           ENDDO
         ENDIF
        ENDIF
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */

      RETURN
      END
