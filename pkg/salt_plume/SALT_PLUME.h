#ifdef ALLOW_SALT_PLUME

C--   SALT_PLUME parameters
C     Find surface where the potential density (ref.lev=surface) is
C     larger than surface density plus SaltPlumeCriterion.

C     SaltPlumeSouthernOcean: TRUE  = apply salt plume globally
C                             FALSE = apply salt plume in Arctic Ocean only
      LOGICAL SaltPlumeSouthernOcean
#ifdef SALT_PLUME_SPLIT_BASIN
      LOGICAL SaltPlumeSplitBasin
#endif
      COMMON /SALT_PLUME_PARAMS_L/ SaltPlumeSouthernOcean
#ifdef SALT_PLUME_SPLIT_BASIN
     &      , SaltPlumeSplitBasin
#endif

C     CriterionType: 1=delta_rho, 2=drhodz, default is 1
C     PlumeMethod: method of distributing salt plume vertically
C       1=power, 2=exp, 3=overshoot, 5=dump_at_top, 6=reverse of 1
C       default is 1
C     Npower: choices of distributing salt uniformly (0), linear (1),
C       or higher power (Npower>1); default is 0 when PlumeMethod = 1
      INTEGER CriterionType, PlumeMethod
#ifdef SALT_PLUME_SPLIT_BASIN
     & , Npower(2)
#else
     & , Npower
#endif
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
C     SPalpha :: fraction of grid volume designated to be brine, [0. 1.]
C       If grid cell 18km x 18km x 10m, take SPalpha=0.001 gives
C       volume of 0.001*drF(1)*[dx*dy] of brine. Thus SPbrineSalt
C       can be calc as adding SaltPlumeFlux into this fractional vol.
C       Default: 0.008 -> SPbrineSalt ~37 if SSS is ~32.
C     SPbrineSconst :: salinity of brine pocket (g/kg)

      _RL SaltPlumeCriterion, SPovershoot
#ifdef SALT_PLUME_SPLIT_BASIN
     &   , SPsalFRAC(2)
#else /* SALT_PLUME_SPLIT_BASIN */
     &   , SPsalFRAC
#endif /* SALT_PLUME_SPLIT_BASIN */
#ifdef SALT_PLUME_VOLUME
     &   , SPbrineSconst, SPbrineSaltmax
#endif /* SALT_PLUME_VOLUME */
#ifdef SALT_PLUME_IN_LEADS
     &   , SPinflectionPoint
#endif /* SALT_PLUME_IN_LEADS */
      COMMON /SALT_PLUME_PARAMS_R/
     &   SPsalFRAC, SaltPlumeCriterion, SPovershoot
#ifdef SALT_PLUME_VOLUME
     &   , SPbrineSconst, SPbrineSaltmax
#endif /* SALT_PLUME_VOLUME */
#ifdef SALT_PLUME_IN_LEADS
     &   , SPinflectionPoint
#endif
C--   SALT_PLUME 2-dim. fields
C     SaltPlumeDepth :: depth of penetration of salt plumes
C                       rejected during sea ice growth
C     saltPlumeFlux :: Net downward salt flux in g/m^2/s
C              Note: only used when salty sea-ice forms.
C              > 0 for increasing in SSS.
C              Southwest C-grid tracer point
C     dSPvolSurf2kLev :: downward volume frac from klev=1 associated w/ saltPlumeFlux
C     dSPvolBelow2kLev:: upward volume frac from grid below (RETIRED)
C     dSPvolkLev2Above:: upward volume frac to grid above
C     SPbrineVolFlux  :: brine Vol associated w/ SPbrineSconst & saltPlumeFlux
C     SPforcingS      :: 3D forcingS associated w/ saltPlumeFlux in g/m^2/s
C     SPforcingT      :: 3D forcingT associated w/ saltPlumeFlux in W/m^2
C     SPkBottom       :: bottom kLev associated with SaltPlumeDepth

      _RL SaltPlumeDepth (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  saltPlumeFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SALT_PLUME_VOLUME
      _RL SPbrineVolFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SPforcS1       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SPforcT1       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dSPvolSurf2kLev (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
     &   ,dSPvolkLev2Above(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
     &   ,SPforcingS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
     &   ,SPforcingT      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
     &   ,SPplumek        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
#endif
      COMMON /DYNVARS_SALT_PLUME/ SaltPlumeDepth
#ifdef SALT_PLUME_VOLUME
     &    ,SPbrineVolFlux,SPforcS1,SPforcT1
      COMMON /FFIELDS_SaltPlumeVol/
     &     dSPvolSurf2kLev,dSPvolkLev2Above
     &     ,SPforcingS,SPforcingT
     &     ,SPplumek
#endif
      COMMON /FFIELDS_saltPlumeFlux/ saltPlumeFlux
#endif /* ALLOW_SALT_PLUME */
