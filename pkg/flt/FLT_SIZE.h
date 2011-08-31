C $Header: /u/gcmpack/MITgcm/pkg/flt/FLT_SIZE.h,v 1.2 2011/08/31 21:41:55 jmc Exp $
C $Name:  $

C     ==================================================================
C     HEADER FLT_SIZE
C     ==================================================================

C== Size ==

C     max_npart_tile :: maximum number of floats on a tile
C                       normally npart_tile < max_npart, but could be the same
C                       for safety
C     max_npart_exch :: maximum number of floats on a tile that can be
C                       exchanged at one time
C                       normally max_npart_exch < max_npart_tile,
C                       but could be the same for safety
C     fltBufDim      :: 1rst dimension of IO buffer, greater than number of
C                       fields to write (used for trajectories & profiles);
C                       can be less than 9+4*Nr if not writing any profile
C                       or only few fields along traj. and vert. profile
      INTEGER max_npart_tile, max_npart_exch, fltBufDim
      PARAMETER (max_npart_tile = 300)
      PARAMETER (max_npart_exch =  50)
      PARAMETER (fltBufDim = (9+4*Nr))
c     PARAMETER (fltBufDim = 13 )
c     PARAMETER (fltBufDim =  8 )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
