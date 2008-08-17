C $Header: /u/gcmpack/MITgcm/pkg/down_slope/DWNSLP_VARS.h,v 1.1 2008/08/17 02:01:27 jmc Exp $
C $Name:  $

#ifdef ALLOW_DOWN_SLOPE

C store the location of potential site where Down-Sloping Flow is applied
C  DWNSLP_NbSite :: Number of bathymetry steps within each tile
C  DWNSLP_ijDeep :: horiz. index of deep water-column next to DWNSLP site
C  DWNSLP_shVsD  :: horiz. index of shallow water-column relative to ijDeep
C  DWNSLP_deepK  :: level index (in deep water column) where dense water
C                   (from the bottom of shallow column) is flowing into.
      INTEGER DWNSLP_NbSite( nSx, nSy )
      INTEGER DWNSLP_ijDeep( DWNSLP_size, nSx, nSy )
      INTEGER DWNSLP_shVsD ( DWNSLP_size, nSx, nSy )
      INTEGER DWNSLP_deepK ( DWNSLP_size, nSx, nSy )
      COMMON /DWNSLP_SITE/
     &  DWNSLP_NbSite, DWNSLP_ijDeep, DWNSLP_shVsD, DWNSLP_deepK

C store variables used to compute the Down-Sloping transport of each tracer.
C     DWNSLP_Gamma  :: geometric factor = slope * cross section area [m^2]
C     DWNSLP_Transp :: down-sloping flow: volume transport [m^3/s]
C        if using P-Coord., Gamma & Transp units become [m.Pa] & [m^2.Pa/s]
      _RL DWNSLP_Gamma ( DWNSLP_size, nSx, nSy )
      _RL DWNSLP_Transp( DWNSLP_size, nSx, nSy )
      COMMON /DWNSLP_FLOW/ DWNSLP_Gamma, DWNSLP_Transp

#endif /* ALLOW_DOWN_SLOPE */
