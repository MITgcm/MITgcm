C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/ZONAL_FILT.h,v 1.4 2001/12/11 14:47:32 jmc Exp $
C $Name:  $

#ifdef ALLOW_ZONAL_FILT

C-    Package flag and logical parameters :
C     zonal_filt_uvStar  :: filter applied to u*,v* (before SOLVE_FOR_P)
C     zonal_filt_TrStagg :: if using a Stager time-step, filter T,S before
C                           computing PhiHyd ; 
C                           has no effect if syncr. time step is used

      LOGICAL zonal_filt_uvStar, zonal_filt_TrStagg
      COMMON /ZONAL_FILT_PARM_L/
     &        zonal_filt_uvStar, zonal_filt_TrStagg

C-    Zonal Filter integer parameters :
C     zonal_filt_cospow  :: Latitude dependance of the damping function
C                           = ( cos Lat / cos zonal_filt_lat )**cospow 
C     zonal_filt_sinpow  :: zonal mode dependance of the damping function
C                           = 1 / ( sin pi.kx/Nx )**sinpow
C     zonal_filt_mode2dx :: to specify how to treat the 2.dx mode :
C                         = 0 : damped like other modes.
C                         = 1 : removed in regions where Zonal_filt apply
C                         = 2 : removed every where.

      INTEGER zonal_filt_cospow, zonal_filt_sinpow, zonal_filt_mode2dx
      COMMON /ZONAL_FILT_PARM_I/
     &        zonal_filt_cospow, zonal_filt_sinpow, zonal_filt_mode2dx
 
C-    Zonal Filter (real) parameters :
C     zonal_filt_lat :: Low latitude for FFT filtering of latitude
C                          circles
      _RL zonal_filt_lat  
      COMMON /ZONAL_FILT_PARAMS/
     & zonal_filt_lat

C Amplitude factor as function of mode number and latitude (U,T points)
      COMMON /ZONAL_FFT/ ampFactor,ampFactorV
C     _RL ampFactor( Nx, 1-Oly:sNy+Oly, nSx, nSy )
C     _RL ampFactorV( Nx, 1-Oly:sNy+Oly, nSx, nSy )
      _RL ampFactor( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly, nSx, nSy )
      _RL ampFactorV( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly, nSx, nSy )

#endif /* ALLOW_ZONAL_FILT */
