C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/FFTPACK.h,v 1.4 2003/10/09 04:19:20 edhill Exp $
C $Name:  $

#include "ZONAL_FILT_OPTIONS.h"

#ifdef ALLOW_ZONAL_FILT

C     Data structures/work-space for FFTPACK
      COMMON /FFTPACKSUPPORT/ FFTPACKWS1,FFTPACKWS2,FFTPACKWS3
C     Real*8 FFTPACKWS(2*Nx+15,nSy)
      Real*8 FFTPACKWS1(Nx,nSy)
      Real*8 FFTPACKWS2(Nx,nSy)
      integer FFTPACKWS3(15,nSy)

#endif /* ALLOW_ZONAL_FILT */
