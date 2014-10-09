C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_weights.h,v 1.11 2014/10/09 00:49:27 gforget Exp $
C $Name:  $

c     Define dummy weights as a placeholder
      common /ctrl_weights_dummy_r/
     &                        wtheta, wsalt,
     &                        wuvvel, wetan, wuvel, wvvel,
     &                        wdiffkr, wkapgm, wkapredi,
     &                        wedtaux, wedtauy

      _RL wtheta   ( nr,nsx,nsy )
      _RL wsalt    ( nr,nsx,nsy )
      _RL wuvvel   ( nr,nsx,nsy )
      _RL wuvel    ( nr,nsx,nsy )
      _RL wvvel    ( nr,nsx,nsy )
      _RL wetan    ( snx,sny,nsx,nsy )
      _RL wdiffkr  ( nr,nsx,nsy )
      _RL wkapgm   ( nr,nsx,nsy )
      _RL wkapredi ( nr,nsx,nsy )
      _RL wedtaux ( nr,nsx,nsy )
      _RL wedtauy ( nr,nsx,nsy )

#if (defined (ALLOW_COST_HFLUXM) || defined (ALLOW_HFLUXM_CONTROL))
c     whfluxm       - weight for heat flux.
      common /cost_weights_r/ whfluxm
      _RL whfluxm (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
#endif

