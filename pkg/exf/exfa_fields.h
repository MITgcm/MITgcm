c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_fields.h,v 1.1 2001/05/14 22:08:41 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exfa_sflx
c     ==================================================================
c
c     o Header file for the surface flux anomaly data.
c       Used by the external forcing package.
c
c     started: Ralf.Giering@FastOpt.de 24-Mai-2000
c
c     ==================================================================
c     HEADER exfa_sflx
c     ==================================================================

c     heat and salt flux.
      _RL hfluxa (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL sfluxa (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)

      common /exfa_flux_r/ hfluxa, sfluxa

c     zonal and meridional wind stress.
      _RL ustressa (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL vstressa (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exfa_stress_r/ ustressa, vstressa
