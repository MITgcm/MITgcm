C $Header: /u/gcmpack/MITgcm/pkg/down_slope/DWNSLP_PARAMS.h,v 1.1 2008/08/17 02:01:27 jmc Exp $
C $Name:  $

#ifdef ALLOW_DOWN_SLOPE

C-    Package flag and logical parameters :
C     temp_useDWNSLP  :: true if Down-Sloping flow applies to temperature
C     salt_useDWNSLP  :: true if Down-Sloping flow applies to salinity
      COMMON /DWNSLP_PARM_L/
     &        temp_useDWNSLP, salt_useDWNSLP
      LOGICAL temp_useDWNSLP, salt_useDWNSLP

C-    Integer parameters :
C     DWNSLP_ioUnit :: ioUnit for log/debug messages
      COMMON /DWNSLP_PARM_I/
     &        DWNSLP_ioUnit
      INTEGER DWNSLP_ioUnit

C-    Down-Sloping Parameterization : real parameters :
C     DWNSLP_slope  :: fixed slope (=0 => use the local slope)
C     DWNSLP_rec_mu :: reciprol friction parameter (unit = time scale [s])
C           used to compute the flow: U=dy*dz*(slope * g/mu * dRho / rho0)
C     dwnslp_drFlow :: max. thickness [m] of the effective downsloping flow layer
      COMMON /DWNSLP_PARAMS_R/
     &        DWNSLP_slope, DWNSLP_rec_mu, DWNSLP_drFlow
      _RL     DWNSLP_slope, DWNSLP_rec_mu, DWNSLP_drFlow

#endif /* ALLOW_DOWN_SLOPE */
