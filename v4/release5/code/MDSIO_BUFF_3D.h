C $Header: /u/gcmpack/MITgcm_contrib/ecco_utils/ecco_v4_release3_devel/code/MDSIO_BUFF_3D.h,v 1.1 2017/05/04 17:46:37 ou.wang Exp $
C $Name:  $

CBOP
C     !ROUTINE: MDSIO_BUFF_3D.h
C     !INTERFACE:
C     include "MDSIO_BUFF_3D.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | MDSIO_BUFF_3D.h
C     | o Shared 3-D Buffers used for I/O
C     *==========================================================*
CEOP

C     size3dBuf  :: buffer 3rd dimension, corresponds to the maximum number
C                   of levels that can be read/written at a time.
C     Note: minimum value = Nr, but in few cases (vertical interpolation,
C           NrPhys from Fizhi, ...)  needs to be larger. Here we pick 2*Nr
C           which should be enough for most applications.
      INTEGER size3dBuf
#ifdef ALLOW_FIZHI
      PARAMETER ( size3dBuf = Nr+NrPhys )
#else
      PARAMETER ( size3dBuf = 2*Nr )
#endif

C--   COMMON /MDS_3D_BUFFERS/  3-D Shared Local Buffers
C     Those buffers have be in common block to be shared by all threads;
C     considered to be "owned" by master-thread and any access by other 
C     than master thread needs to be put protected by BARRIER.
C shared3dBuf_rx :: Heap storage buffer to which master thread
C                   read-in/write-from data which all threads copy from
C                   (during read) or copy to (during write).
      COMMON /MDS_3D_BUFFERS/ shared3dBuf_r8, shared3dBuf_r4
      Real*8 shared3dBuf_r8( sNx*sNy*size3dBuf*nSx*nSy )
      Real*4 shared3dBuf_r4( sNx*sNy*size3dBuf*nSx*nSy )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
