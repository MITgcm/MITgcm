C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/ZONAL_FILT.h,v 1.3 2001/02/04 14:38:50 cnh Exp $
C $Name:  $

#ifdef ALLOW_ZONAL_FILT

C Amplitide factor as function of mode number and latitude (U,T points)
      COMMON /ZONAL_FFT/ ampFactor,ampFactorV
C     _RL ampFactor( Nx, 1-Oly:sNy+Oly, nSx, nSy )
C     _RL ampFactorV( Nx, 1-Oly:sNy+Oly, nSx, nSy )
      _RL ampFactor( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly, nSx, nSy )
      _RL ampFactorV( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly, nSx, nSy )

#endif /* ZONAL_FILT */
