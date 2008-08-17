C $Header: /u/gcmpack/MITgcm/pkg/down_slope/DWNSLP_SIZE.h,v 1.1 2008/08/17 02:01:27 jmc Exp $
C $Name:  $

c#ifdef ALLOW_DOWN_SLOPE

C-    dimension parameters :
C     xSize  :: X-dimension for 1 tile array (overlap included)
C     xySize :: product of X-dim x Y-dim for 1 tile array
      INTEGER xSize, xySize
      PARAMETER (  xSize = Olx+sNx+Olx )
      PARAMETER ( xySize = xSize*(Oly+sNy+Oly) )

C     DWNSLP_size :: size of downsloping-flow arrays ; for each tile,
C            must be > number of bathymetric steps
C            The default value (=tile domain size) can increased (if needed)
C             or reduced (to save memory space).
      INTEGER DWNSLP_size
      PARAMETER ( DWNSLP_size = xSize * (Oly+sNy+Oly) )

c#endif /* ALLOW_DOWN_SLOPE */
