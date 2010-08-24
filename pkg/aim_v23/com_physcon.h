C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_physcon.h,v 1.3 2010/08/24 13:29:00 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /PHYCON/: Physical constants (initial. in INPHYS)
C       P0    = reference pressure                 [Pa=N/m2]
C       GG    = gravity accel.                     [m/s2]
C       RD    = gas constant for dry air           [J/kg/K]
C       CP    = specific heat at constant pressure [J/kg/K]
C       ALHC  = latent heat of condensation        [J/g]
C       ALHF  = latent heat of freezing            [J/g]
C       SBC   = Stefan-Boltzmann constant
C     rainCP  = heat capacity of liquid water      [J/g/K]
C     tFreeze = freezing temperature of pure water [K]
      COMMON /PHYCON/ P0, GG, RD, CP, ALHC, ALHF, SBC, rainCP, tFreeze
      _RL P0, GG, RD, CP, ALHC, ALHF, SBC, rainCP, tFreeze

C--   COMMON /FSIGLT/: Functions of sigma and latitude (initial. in INPHYS)
C     (execpt SLAT & CLAT init. in  aim_dyn2aim.F)
C      SIG    = full-level sigma
C      SIGL   = logarithm of full-level sigma
C      SIGH   = half-level sigma
C      DSIG   = layer depth in sigma
C      POUT   = norm. pressure level [p/p0] for post-processing
C      GRDSIG = g/(d_sigma p0) : to convert fluxes of u,v,q into d(u,v,q)/dt
C      GRDSCP = g/(d_sigma p0 c_p): to convert energy fluxes into dT/dt
C      WVI    = weights for vertical interpolation
c      SLAT   = sin(lat)
c      CLAT   = cos(lat)
      COMMON /FSIGLT/
     &    SIG, SIGL, SIGH, DSIG, GRDSIG, GRDSCP, WVI
c    &    SIG, SIGL, SIGH, DSIG, POUT, GRDSIG, GRDSCP, WVI, SLAT, CLAT
      _RL SIG(NLEV)
      _RL SIGL(NLEV)
      _RL SIGH(0:NLEV)
      _RL DSIG(NLEV)
c     _RL POUT(NLEV)
      _RL GRDSIG(NLEV)
      _RL GRDSCP(NLEV)
      _RL WVI(NLEV,2)
c     _RL SLAT(NLAT)
c     _RL CLAT(NLAT)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */
