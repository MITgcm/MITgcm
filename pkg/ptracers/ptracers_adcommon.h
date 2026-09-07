!--   These common blocks are extracted from the
!--   automatically created tangent linear code.
!--   You need to make sure that they are up-to-date
!--   (i.e. in right order), and customize them
!--   accordingly.
!--
!--   heimbach@mit.edu 11-Jan-2001

#ifdef ALLOW_AUTODIFF_MONITOR

#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif

#ifdef ALLOW_PTRACERS
      _RL adgptr(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,                          &
     &      ptracers_num)
      _RL adgptrnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,                       &
     &      ptracers_num)
      _RL adptracer(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,                       &
     &      ptracers_num)
      _RL adsurfaceforcingptr(1-olx:snx+olx,1-oly:sny+oly,                        &
     &      nsx,nsy,ptracers_num)
      common /adptracers_fields/ adptracer, adgptr, adgptrnm1,                    &
     &      adsurfaceforcingptr
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
