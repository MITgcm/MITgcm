C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/EXF_CLIM_FIELDS.h,v 1.1 2007/04/16 23:27:20 jmc Exp $
C $Name:  $
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
      common /exf_clim_sst_r/ climsst, lambda_climsst,
     &                        climsst0, climsst1
      _RL climsst       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lambda_climsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL climsst0      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL climsst1      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      common /exf_clim_sss_r/ climsss, lambda_climsss,
     &                        climsss0, climsss1
      _RL climsss       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL lambda_climsss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL climsss0      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL climsss1      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

#ifdef ALLOW_CLIMTEMP_RELAXATION
      common /exf_clim_temp_r/ climtemp, lambda_climtemp,
     &                         climtemp0, climtemp1
      _RL climtemp       (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL lambda_climtemp(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL climtemp0      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL climtemp1      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      common /exf_clim_salt_r/ climsalt, lambda_climsalt,
     &                         climsalt0, climsalt1
      _RL climsalt       (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL lambda_climsalt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL climsalt0      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL climsalt1      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
