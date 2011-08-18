C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_adcommon.h,v 1.1 2011/08/18 09:11:00 heimbach Exp $
C $Name:  $

C--   These common blocks are extracted from the
C--   automatically created tangent linear code.
C--   You need to make sure that they are up-to-date
C--   (i.e. in right order), and customize them
C--   accordingly.
C--
C--   heimbach@mit.edu 11-Jan-2001

#ifdef ALLOW_AUTODIFF_MONITOR

#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif

#ifdef ALLOW_PTRACERS
      _RL adgptr(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,
     $ptracers_num)
      _RL adgptrnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,
     $ptracers_num)
      _RL adptracer(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,
     $ptracers_num)
      _RL adsurfaceforcingptr(1-olx:snx+olx,1-oly:sny+oly,
     $nsx,nsy,ptracers_num)
      common /adptracers_fields/ adptracer, adgptr, adgptrnm1, 
     $adsurfaceforcingptr
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
