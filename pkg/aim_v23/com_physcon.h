C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_physcon.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /PHYCON/: Physical constants (initial. in INPHYS)
C      P0   = reference pressure
C      GG   = gravity accel.
C      RD   = gas constant for dry air
C      CP   = specific heat at constant pressure
C      ALHC = latent heat of condensation
C      SBC  = Stefan-Boltzmann constant
      COMMON /PHYCON/ P0, GG, RD, CP, ALHC, SBC
      _RL P0, GG, RD, CP, ALHC, SBC

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
