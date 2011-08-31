C $Header: /u/gcmpack/MITgcm/pkg/flt/FLT_BUFF.h,v 1.1 2011/08/31 21:41:55 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: FLT_BUFF.h
C     !INTERFACE:
C     #include FLT_BUFF.h

C     !DESCRIPTION:
C     *==========================================================*
C     | FLT_BUFF.h
C     | o Shared FLT Buffers used for I/O
C     *==========================================================*
CEOP

C     flt_io_buff :: IO buffer used for writing trajectories & profiles
      COMMON / FLT_IO_BUFF / flt_io_buff
      _RL     flt_io_buff(fltBufDim,max_npart_tile,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
