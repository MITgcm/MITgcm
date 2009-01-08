C $Header: /u/gcmpack/MITgcm/pkg/flt/FLT.h,v 1.2 2009/01/08 19:40:41 jmc Exp $
C $Name:  $

C     ==================================================================
C     HEADER flt
C     ==================================================================
C
C     o This header file contains variables that are used by the
C       flt package.
C
C     ==================================================================
C     HEADER flt
C     ==================================================================

C== Size ==

C     max_npart_tile :: maximum number of floats on a tile
C                       normally npart_tile < max_npart, but could be the same
C                       for safety
C     max_npart_exch :: maximum number of floats on a tile that can be
C                       exchanged at one time
C                       normally max_npart_exch < max_npart_tile,
C                       but could be the same for safety
C
      INTEGER max_npart_tile, max_npart_exch
      PARAMETER (max_npart_tile = 300)
      PARAMETER (max_npart_exch =  50)

C== Constant ==

C     flt_surf    :: "surface" level of the floats.
C                    Can be deeper than 1, e.g. IF no mixed layer model is used
C     flt_nan     :: qualifier for a float that is not yet released or that died
      _RL flt_surf
      _RL flt_nan
      PARAMETER (flt_surf = 1.)
      PARAMETER (flt_nan = -999.)

C     Full domain extension:
C     flt_xLo     :: Full domain lower X boundary
C     flt_xHi     :: Full domain upper X boundary
C     flt_yLo     :: Full domain lower Y boundary
C     flt_yHi     :: Full domain upper X boundary
C
      _RL    flt_xLo, flt_xHi, flt_yLo, flt_yHi
      COMMON / FLT_CONST_R /
     &       flt_xLo, flt_xHi, flt_yLo, flt_yHi

C== Parameters ==

C     flt_noise   :: range of noise added to the velocity component
C                    (randomly). The noise can be added or subtracted,
C                    the range is +/- flt_noise/2
C    flt_int_traj :: period between storing model state at float position, in s
C    flt_int_prof :: period between float vertical profiles, in s
C
C    note:  flt_int_prof is the time between getting profiles, not the
C           the return  cycle of the float to the surface. The latter can
C           be specified individually for every float. Because the mechanism
C           for return ing to the surface is called in the profiling routine
C           flt_int_prof has to be the minimum of all iup(max_npart).
C           The subsampling of profiles can be done later in the analysis.
C
      _RL flt_noise
      _RL flt_int_traj, flt_int_prof
      COMMON / FLT_PARAM_R /
     &       flt_noise,
     &       flt_int_traj, flt_int_prof

C     flt_file    :: name of the file containing the initial positions.
C                    At initialization the program first looks for a
C                    global file flt_file.data. If that is not found it
C                    looks for tiled files flt_file.iG.jG.data.
C
      CHARACTER*(MAX_LEN_FNAM) flt_file
      COMMON / FLT_PARAM_C / flt_file

C== Float State ==

C     npart_tile  :: actual number of floats per tile
C
      INTEGER npart_tile(nSx,nSy)
      COMMON / FLT_STATE_I / npart_tile

C     max_npart   :: total number of floats
C                    (this is read from the input files)
C
      _RL   max_npart
      COMMON / FLT_STATE_R / max_npart

C     npart       :: float identifier
C     tstart      :: start date of integration of float, in s.
C                    Note: If tstart=-1 floats are integrated right from the
C                          beginning
C     tend        :: end date of integration of float, in s.
C                    Note: If tend=-1 floats are integrated till the end of
C                          integration
C     xpart       :: x position of float (in units of XC)
C     ypart       :: y position of float (in units of YC)
C     kpart       :: vertical level of float (might be decimal
C                    number in case of 3D floats)
C     kfloat      :: target depth of float.
C     iup         :: flag IF the float
C                    should profile   ( >  0 = RETURN  cycle, in s, to surface)
C                    remain at depth  ( =  0 )
C                    is a 3D float    ( = -1 )
C                    should be advected WITHOUT additional noise ( = -2 ).
C                    This implies that the float is non-profiling
C                    is a mooring     ( = -3 ), i.e. the float is not advected
C     itop        :: time of float the surface, in s
      _RL     npart(max_npart_tile,nSx,nSy)
      _RL    tstart(max_npart_tile,nSx,nSy)
      _RL      tend(max_npart_tile,nSx,nSy)
      _RL     xpart(max_npart_tile,nSx,nSy)
      _RL     ypart(max_npart_tile,nSx,nSy)
      _RL     kpart(max_npart_tile,nSx,nSy)
      _RL    kfloat(max_npart_tile,nSx,nSy)
      _RL       iup(max_npart_tile,nSx,nSy)
      _RL      itop(max_npart_tile,nSx,nSy)

      COMMON / FLT_STATE_VAR /
     &       npart, tstart, tend,
     &       xpart, ypart, kpart,
     &       kfloat, iup, itop

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
