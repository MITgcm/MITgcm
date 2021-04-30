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
C--   COMMON /MDS_WH_BUFFERS/  Shared Local Buffers With Halos
C     Those buffers have be in common block to be shared by all threads
C fld2d_procbuff_rx :: Heap storage buffer to which master thread
C                   read-in/write-from data which all threads copy from
C                   (during read) or copy to (during write).
      COMMON /MDS_WH_BUFFERS/
#ifndef EXCLUDE_WHIO_GLOBUFF_2D
     &                        fld2d_globuff_r8, fld2d_globuff_r4,
#endif
     &                        fld2d_procbuff_r8, fld2d_procbuff_r4
      Real*8 fld2d_procbuff_r8( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy )
      Real*4 fld2d_procbuff_r4( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy )
#ifndef EXCLUDE_WHIO_GLOBUFF_2D
      Real*4 fld2d_globuff_r4( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy*nPx*nPy )
      Real*8 fld2d_globuff_r8( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy*nPx*nPy )
#endif

#ifdef ALLOW_WHIO_3D
      COMMON /MDS_WH_BUFFERS_3D_L/ writeWh
      logical writeWh
      COMMON /MDS_WH_BUFFERS_3D_I/ iWh, jWh
      integer nWh, iWh, jWh
      PARAMETER (nWh=618)
      COMMON /MDS_WH_BUFFERS_3D_RL/
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
     &                        fld3d_globuff_r8, fld3d_globuff_r4,
#  endif
     &                        fld3d_procbuff_r8, fld3d_procbuff_r4
      Real*8 fld3d_procbuff_r8
     &       ( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy*nWh )
      Real*4 fld3d_procbuff_r4
     &       ( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy*nWh )
#  ifdef INCLUDE_WHIO_GLOBUFF_3D
      Real*4 fld3d_globuff_r4
     &       ( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy*nPx*nPy*nWh )
      Real*8 fld3d_globuff_r8
     &       ( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy*nPx*nPy*nWh )
#  endif
#endif

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
