C $Header: /u/gcmpack/MITgcm/pkg/zonal_filt/FFTPACK.h,v 1.2 2001/02/02 21:36:30 adcroft Exp $

#include "CPP_OPTIONS.h"

C     Data structures/work-space for FFTPACK
      COMMON /FFTPACKSUPPORT/ FFTPACKWS1,FFTPACKWS2,FFTPACKWS3
C     Real*8 FFTPACKWS(2*Nx+15,nSy)
      Real*8 FFTPACKWS1(Nx,nSy)
      Real*8 FFTPACKWS2(Nx,nSy)
      integer FFTPACKWS3(15,nSy)
