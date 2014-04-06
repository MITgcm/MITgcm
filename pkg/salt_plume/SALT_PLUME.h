C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/SALT_PLUME.h,v 1.9 2014/04/06 09:34:00 atn Exp $
C $Name:  $

#ifdef ALLOW_SALT_PLUME

C--   SALT_PLUME parameters
C     Find surface where the potential density (ref.lev=surface) is
C     larger than surface density plus SaltPlumeCriterion.

C     SaltPlumeSouthernOcean: TRUE  = apply salt plume globally
C                             FALSE = apply salt plume in Arctic Ocean only
      LOGICAL SaltPlumeSouthernOcean
      COMMON /SALT_PLUME_PARAMS_L/ SaltPlumeSouthernOcean

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
C     SPsalFRAC: fraction of the salt by-product of seaice growth (not melt) that
C       will be re-distributed vertically according to the salt_plume_frac.F
C       Its default is 1. (for 100% effect), and its range is [0. 1.]
C     SPinflectionPoint: the inflection point of a nonlinear function 
C       f(AREA) controlling saltPlumeFlux. f(AREA) is a logistic curve
C       (sigmoid) with range [0. 1.] and f(SPinflectionPoint) == 0.5.
C       Usage: pkg/salt_plume activates when AREA>=SPinflectionPoint.
C       To assure only narrow leads generate plumes: 
C       set SPinflectionPoint >= 0.8.

      _RL SPsalFRAC, SaltPlumeCriterion, SPovershoot
#ifdef SALT_PLUME_IN_LEADS
      _RL SPinflectionPoint
#endif
      COMMON /SALT_PLUME_PARAMS_R/
     &   SPsalFRAC, SaltPlumeCriterion, SPovershoot
#ifdef SALT_PLUME_IN_LEADS
     &   , SPinflectionPoint
#endif
C--   SALT_PLUME 2-dim. fields
C     SaltPlumeDepth :: depth of penetration of salt plumes
C                       rejected during sea ice growth
C     saltPlumeFlux :: Net downward salt flux in psu.kg/m^2/s
C              Note: a) only used when salty sea-ice forms.
C                    b) units: when salinity (unit= psu) is expressed
C                       in g/kg, saltPlumeFlux unit becomes g/m^2/s.
C              > 0 for increasing in SSS.
C              Southwest C-grid tracer point
      _RL SaltPlumeDepth (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  saltPlumeFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /DYNVARS_SALT_PLUME/ SaltPlumeDepth
      COMMON /FFIELDS_saltPlumeFlux/ saltPlumeFlux

#endif /* ALLOW_SALT_PLUME */
