C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/SALT_PLUME.h,v 1.5 2007/12/21 22:49:09 atn Exp $
C $Name:  $

#ifdef ALLOW_SALT_PLUME

C--   SALT_PLUME parameters
C     Find surface where the potential density (ref.lev=surface) is
C     larger than surface density plus SaltPlumeCriterion.

C     CriterionType: 1=delta_rho, 2=drhodz, default is 1
C     PlumeMethod: method of distributing salt plume vertically
C       1=power, 2=exp, 3=overshoot, 5=dump_at_top, 6=reverse of 1
C       default is 1
C     Npower: choices of distributing salt uniformly (0), linear (1),
C       or higher power (Npower>1); default is 0 when PlumeMethod = 1
      INTEGER CriterionType, PlumeMethod, Npower
      COMMON /SALT_PLUME_PARAMS_I/ CriterionType, PlumeMethod, Npower

C     SaltPlumeCriterion
C       for CriterionType=1, default is 0.4 kg/m^3 of Duffy et al 1999
C       for CriterionType=2, default is 0.005 kg/m^3/m
C     SPovershoot: overshooting depth of penetrating salt plume,
C       so that 1.0 = no-overshoot, 1.2 = 20% overshoot.
C       default is 1.0
      _RL SaltPlumeCriterion, SPovershoot
      COMMON /SALT_PLUME_PARAMS_R/ SaltPlumeCriterion, SPovershoot

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
