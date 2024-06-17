CBOP
C     !ROUTINE: MDSIO_BUFF_WH.h
C     !INTERFACE:
C     include "MDSIO_BUFF_WH.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | MDSIO_BUFF_WH.h
C     | o Shared Buffers used for I/O WITH HALOS
C     *==========================================================*
CEOP

#ifdef ALLOW_WHIO
C--   COMMON /MDS_WH_BUFFERS_R[8,4]/  Shared Local Buffers With Halos
C     Those buffers have be in common block to be shared by all threads
C fld2d_procbuff_rx :: Heap storage buffer to which master thread
C                   read-in/write-from data which all threads copy from
C                   (during read) or copy to (during write).
      COMMON /MDS_WH_BUFFERS_R8/ fld2d_procbuff_r8
# ifndef EXCLUDE_WHIO_GLOBUFF_2D
     &                         , fld2d_globuff_r8
# endif
      COMMON /MDS_WH_BUFFERS_R4/ fld2d_procbuff_r4
# ifndef EXCLUDE_WHIO_GLOBUFF_2D
     &                         , fld2d_globuff_r4
# endif
      Real*8 fld2d_procbuff_r8( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy )
      Real*4 fld2d_procbuff_r4( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy )
# ifndef EXCLUDE_WHIO_GLOBUFF_2D
      Real*8 fld2d_globuff_r8( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy*nPx*nPy )
      Real*4 fld2d_globuff_r4( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy*nPx*nPy )
# endif

# ifdef ALLOW_WHIO_3D
      COMMON /MDS_WH_BUFFERS_3D_L/ writeWh
      LOGICAL writeWh
      COMMON /MDS_WH_BUFFERS_3D_I/ iWh, jWh
      INTEGER nWh, iWh, jWh
      PARAMETER (nWh=633)
      COMMON /MDS_WH_BUFFERS_3D_R8/ fld3d_procbuff_r8
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
     &                            , fld3d_globuff_r8
#  endif
      COMMON /MDS_WH_BUFFERS_3D_R4/ fld3d_procbuff_r4
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
     &                            , fld3d_globuff_r4
#  endif
      Real*8 fld3d_procbuff_r8
     &       ( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy*nWh )
      Real*4 fld3d_procbuff_r4
     &       ( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy*nWh )
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
      Real*8 fld3d_globuff_r8
     &       ( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy*nPx*nPy*nWh )
      Real*4 fld3d_globuff_r4
     &       ( (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy*nPx*nPy*nWh )
#  endif
# endif /* ALLOW_WHIO_3D */

#endif /* ALLOW_WHIO */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
