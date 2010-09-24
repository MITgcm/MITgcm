C $Header: /u/gcmpack/MITgcm/pkg/mdsio/MDSIO_BUFF_WH.h,v 1.1 2010/09/24 18:39:35 gforget Exp $
C $Name:  $

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

C--   COMMON /MDS_WH_BUFFERS/  Shared Local Buffers With Halos
C     Those buffers have be in common block to be shared by all threads
C fld2d_procbuff_rx :: Heap storage buffer to which master thread
C                   read-in/write-from data which all threads copy from
C                   (during read) or copy to (during write).
      COMMON /MDS_WH_BUFFERS/ fld2d_procbuff_r8, fld2d_procbuff_r4
      Real*8 fld2d_procbuff_r8( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy )
      Real*4 fld2d_procbuff_r4( (sNx+2*Olx)*(sNy+2*Oly)*nSx*nSy )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
