C $Header: /u/gcmpack/MITgcm/pkg/flt/FLT.h,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

c     ==================================================================
c     HEADER flt
c     ==================================================================
c
c     o This header file contains variables that are used by the
c       flt package. 
c
c     ==================================================================
c     HEADER flt
c     ==================================================================
c
c
c     max_npart       - total number of floats
c                       (this is read from the input files)
c
      _RL   max_npart
      common /flt_n/ max_npart


c     max_npart_tile  - maximum number of floats on a tile
c                       normally npart_tile < max_npart, but could be the same
c                       for safety
c     max_npart_exch  - maximum number of floats on a tile that can be
c                       exchanged at one time
c                       normally max_npart_exch < max_npart_tile,
c                       but could be the same for safety
c
      INTEGER max_npart_tile, max_npart_exch
      parameter (max_npart_tile = 300)
      parameter (max_npart_exch =  50)

c     npart_tile   - actual number of floats per tile
c
      INTEGER npart_tile(nSx,nSy)
      common /flt_t/ npart_tile

c     npart        - float identifier
c     xpart        - x position of float (in units of XC)
c     ypart        - y position of float (in units of YC)
c     kpart        - vertical level of float (might be decimal
c                    number in case of 3D floats)
c     tstart       - start date of integration of float, in s.
c                    Note: If tstart=-1 floats are integrated right from the 
c                          beginning
c     tend         - end   date of integration of float, in s.
c                    Note: If tend=-1 floats are integrated till the end of
c                          integration
c     kfloat       - target depth of float.
c     iup          - flag if the float 
c                    should profile   ( >  0 = return cycle, in s, to surface) 
c                    remain at depth  ( =  0 )
c                    is a 3D float    ( = -1 ).
c                    should be advected WITHOUT additional noise ( = -2 ). 
c                    This implies that the float is non-profiling
c                    is a mooring     ( = -3 ), i.e. the float is not advected
c     itop         - time of float the surface, in s
c
      _RL     npart(max_npart_tile,nSx,nSy)
      _RL    tstart(max_npart_tile,nSx,nSy)
      _RL      tend(max_npart_tile,nSx,nSy)
      _RL     xpart(max_npart_tile,nSx,nSy)
      _RL     ypart(max_npart_tile,nSx,nSy)
      _RL     kpart(max_npart_tile,nSx,nSy)
      _RL    kfloat(max_npart_tile,nSx,nSy)
      _RL       iup(max_npart_tile,nSx,nSy)
      _RL      itop(max_npart_tile,nSx,nSy)

      common /flt_v/ npart,tstart,tend,xpart,ypart,kpart,kfloat,iup,itop

c     flt_int_traj - period between storing model state at float position, in s
c     flt_int_prof - period between float vertical profiles, in s
c
c     note: flt_int_prof is the time between getting profiles, NOT the
c           the return cycle of the float to the surface. The latter can
c           be specified individually for every float. Because the mechanism
c           for returning to the surface is called in the profiling routine
c           flt_int_prof has to be the minimum of all iup(max_npart).
c           The subsampling of profiles can be done later in the analysis.
c
      _RL flt_int_traj, flt_int_prof
      common /flt_param_i/ flt_int_traj, flt_int_prof

c      flt_surf - "surface" level of the floats.
c                  Can be deeper than 1, e.g. if no mixed layer model is used
      _RL flt_surf
      parameter (flt_surf = 1.)

c     flt_nan    - qualifier for a float that is not yet released or that died
c
      _RL flt_nan
      parameter (flt_nan = -999.)

c     flt_noise - range of noise added to the velocity component
c                 (randomly). The noise can be added or subtracted,
c                 the range is +/- flt_noise/2
c
      _RL flt_noise
      common /flt_param_n/ flt_noise

c     flt_file  - name of the file containing the initial/restart positions.
c                 At initialization the program first looks for a global
c                 file flt_file.data. If that is not found it looks
c                 for  flt_file.iG.jG.data. That is also written as a restart
c
      CHARACTER*(MAX_LEN_FNAM) flt_file
      common /flt_param_d/ flt_file
