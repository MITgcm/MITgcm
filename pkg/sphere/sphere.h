
c     ==================================================================
c     HEADER SPHERE
c     ==================================================================
c
c     o Parameters for spherical harmonic analysis of sea surface height
c       field.
c       Assumes regular grid with 2 degree meridional spacing!
c     o TODO: interpolate model grid to regular grid
c
c     started: Ralf Giering Jul-1996
c
c     changed: Christian Eckert eckert@mit.edu  29-Feb-2000
c
c     changed: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c
c     ==================================================================
c     HEADER SPHERE
c     ==================================================================


c     Dimensions in spherical harmonic space.
c     =======================================
c
c     lshcmax   -  maximal degree l of spherical harmonics to be
c                  projected on in a spherical harmonic analysis.
c     ncshc     -  number of spherical harmonics coefficients correspond-
c                  ing to lshmax.
c     lonshc    -  zonal      number of grid points of regular grid
c     latshc    -  meridional number of grid points of regular grid
c     dthetashc -  zonal      grid spacing
c     dphishc   -  meridional grid spacing

      integer    lshcmax
      parameter( lshcmax   = 70 )

      integer    ncshc
      parameter( ncshc   = (lshcmax+1)*(lshcmax+1) )

      _RL        dthetashc
      parameter( dthetashc = 2. )

      _RL        dphishc
      parameter( dphishc   = 2. )

      integer    lonshc
      parameter( lonshc    = Nx )

      integer    latshc
      parameter( latshc    = 160.0/dphishc )

c     ==================================================================
c     END OF HEADER SPHERE
c     ==================================================================


