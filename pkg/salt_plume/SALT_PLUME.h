C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/SALT_PLUME.h,v 1.3 2007/11/28 17:55:36 dimitri Exp $
C $Name:  $

#ifdef ALLOW_SALT_PLUME

C--   SALT_PLUME parameters
C     Find surface where the potential density (ref.lev=surface) is
C     larger than surface density plus SaltPlumeCriterion.  Initially
C     use the default value 0.4 kg/m^3 of Duffy et al, 1999.
      _RL SaltPlumeCriterion
      COMMON /SALT_PLUME_PARAMS_R/ SaltPlumeCriterion

C--   SALT_PLUME 2-dim. fields
C     SaltPlumeDepth :: depth of penetration of salt plumes
C                       rejected during sea ice growth
C     saltPlumeFlux :: Net downward salt flux in psu.kg/m^2/s
C              Note: a) only used when salty sea-ice forms.
C                    b) units: when salinity (unit= psu) is expressed
C              	        in g/kg, saltPlumeFlux unit becomes g/m^2/s.
C              > 0 for increasing in SSS.
C              Southwest C-grid tracer point
      _RL SaltPlumeDepth (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  saltPlumeFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /DYNVARS_SALT_PLUME/ SaltPlumeDepth
      COMMON /FFIELDS_saltPlumeFlux/ saltPlumeFlux

#endif /* ALLOW_SALT_PLUME */
