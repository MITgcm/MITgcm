C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/com_forcing1.h,v 1.3 2001/02/04 14:38:49 cnh Exp $
C $Name:  $
C $Namer: $

C--   /LSMASK/ land-sea masks 
      common /LSMASK/ fmask1(ngp)	! fractional land-sea mask

C--   /FORFIX/ Time invariant forcing fields 
      common /FORFIX/ phi0(ngp),	! surface geopotential
     .                alb0(ngp)		! land-surface albedo

C--   /FORCIN/ Forcing fields 
      common /FORCIN/ sst1(ngp),	! SST
     .                oice1(ngp),	! sea ice fraction
     .                stl1(ngp),	! land-surface temperature
     .                snow1(ngp),	! snow depth (mm water)
     .                soilq1(ngp)	! soil wetness (mm water)
