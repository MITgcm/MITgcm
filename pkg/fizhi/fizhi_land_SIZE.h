C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_land_SIZE.h,v 1.3 2004/06/09 20:33:37 molod Exp $
C $Name:  $

c Land Grid Horizontal Dimension (Number of Tiles)
c ------------------------------------------------
      integer nchp, maxtyp
      parameter (maxtyp = 10)
      parameter (nchp = sNx*sNy*maxtyp)
