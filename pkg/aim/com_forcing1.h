C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_forcing1.h,v 1.4 2001/05/29 19:28:53 cnh Exp $
C $Name:  $
C $Namer: $

C--   /LSMASK/ land-sea masks 
      common /LSMASK/ fmask1(ngp,MAX_NO_THREADS) ! fractional land-sea mask

C--   /FORFIX/ Time invariant forcing fields 
      common /FORFIX/ phi0(ngp,MAX_NO_THREADS),	 ! surface geopotential
     .                alb0(ngp,MAX_NO_THREADS)	 ! land-surface albedo

C--   /FORCIN/ Forcing fields 
      common /FORCIN/ sst1(ngp,MAX_NO_THREADS),	 ! SST
     .                oice1(ngp,MAX_NO_THREADS), ! sea ice fraction
     .                stl1(ngp,MAX_NO_THREADS),	 ! land-surface temperature
     .                snow1(ngp,MAX_NO_THREADS), ! snow depth (mm water)
     .                soilq1(ngp,MAX_NO_THREADS) ! soil wetness (mm water)
