c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_fields.h,v 1.1 2001/05/14 22:08:40 heimbach Exp $
c
c
c     ==================================================================
c     HEADER exf_clim
c     ==================================================================
c
c     o Header for the climatology part of the external forcing package.
c
c     started: Ralf Giering 15-Jan-2001
c
c     ==================================================================
c     HEADER exf_clim
c     ==================================================================

c     Model fields.

#ifdef ALLOW_CLIMSST_RELAXATION
      common /exf_clim_sst_r/ climsst, lambda_climsst
      _RL climsst       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lambda_climsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      common /exf_clim_sss_r/ climsss, lambda_climsss
      _RL climsss       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lambda_climsss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ALLOW_CLIMTEMP_RELAXATION
      common /exf_clim_temp_r/ climtemp, lambda_climtemp
      _RL climtemp       (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL lambda_climtemp(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      common /exf_clim_salt_r/ climsalt, lambda_climsalt
      _RL climsalt       (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL lambda_climsalt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
