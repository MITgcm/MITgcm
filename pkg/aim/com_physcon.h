C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_physcon.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

C--
C--   /PHYCON/: Physical constants (initial. in INPHYS)
C--    P0   = reference pressure
C--    GG   = gravity accel.
C--    RD   = gas constant for dry air
C--    CP   = specific heat at constant pressure
C--    ALHC = latent heat of condensation
C--    SBC  = Stefan-Boltzmann constant

      COMMON /PHYCON/ P0, GG, RD, CP, ALHC, SBC
C--
C--   /FSIGMU/: Functions of sigma and latitude (initial. in INPHYS)
C--    SIG    = full-level sigma 
C--    SIGL   = logarithm of full-level sigma
C--    SIGH   = half-level sigma
C--    DSIG   = layer depth in sigma
C--    POUT   = norm. pressure level [p/p0] for post-processing
C--    GRDSIG = g/(d_sigma p0) : to convert fluxes of u,v,q into d(u,v,q)/dt
C--    GRDSCP = g/(d_sigma p0 c_p): to convert energy fluxes into dT/dt
C--    WVI    = weights for vertical interpolation
C--    FMU    = legendre polinomials in sin(lat)

      COMMON /FSIGMU/ SIG(NLEV), SIGL(NLEV), SIGH(0:NLEV), DSIG(NLEV),
     *                POUT(NLEV), GRDSIG(NLEV), GRDSCP(NLEV), 
     *                WVI(NLEV,2), FMU(NLAT,2)
