C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/SALT_PLUME.h,v 1.1 2007/11/28 00:18:17 dimitri Exp $
C $Name:  $

C     SaltPlumeDepth :: depth of penetration of salt plumes
C                       rejected during sea ice growth
      COMMON /DYNVARS_SALT_PLUME/ SaltPlumeDepth
      _RL SaltPlumeDepth (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C saltPlumeFlux :: Net downward salt flux in psu.kg/m^2/s
C              flux of Salt rejected back into the ocean per time unit (second).
C              Note: a) only used when salty sea-ice forms or melts.
C                    b) units: when salinity (unit= psu) is expressed
C              	        in g/kg, saltPlumeFlux unit becomes g/m^2/s.
C              > 0 for increasing in SSS.
C              Southwest C-grid tracer point
      COMMON /FFIELDS_saltPlumeFlux/ saltPlumeFlux
      _RL  saltPlumeFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
