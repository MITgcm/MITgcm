C $Header: /u/gcmpack/MITgcm/pkg/chronos/chronos.h,v 1.1 2004/07/28 01:26:03 molod Exp $
C $Name:  $

C **********************************************************************
C *****                   Clock Variables                          *****
C **********************************************************************

      character*132     tags
      integer          ntags
      integer          nymd,nhms
      integer          nymd0,nhms0
      integer          freqs,dates,times

      integer          maxtag
      parameter       (maxtag=500)

      common /chronos/ nymd,nhms
      common /chronos/ nymd0,nhms0
      common /chronos/ ntags
      common /chronos/  tags (maxtag) 
      common /chronos/  freqs(maxtag) 
      common /chronos/  dates(maxtag) 
      common /chronos/  times(maxtag) 

