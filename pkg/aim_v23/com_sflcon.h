C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_sflcon.h,v 1.2 2004/06/24 23:41:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /SFLCON/: Constants for surface fluxes (initial. in INPHYS)
C      FWIND0 = ratio of near-sfc wind to lowest-level wind
C      FTEMP0 = weight for near-sfc temperature extrapolation (0-1) :
C               1 : linear extrapolation from two lowest levels
C               0 : constant potential temperature ( = lowest level)
C      FHUM0  = weight for near-sfc specific humidity extrapolation (0-1) :
C               1 : extrap. with constant relative hum. ( = lowest level)
C               0 : constant specific hum. ( = lowest level)
C      CDL    = drag coefficient for momentum over land
C      CDS    = drag coefficient for momentum over sea
C      CHL    = heat exchange coefficient over land
C      CHS    = heat exchange coefficient over sea
C      VGUST  = wind speed for sub-grid-scale gusts
C      CTDAY  = daily-cycle correction (dTskin/dSSRad)
C      DTHETA = Potential temp. gradient for stability correction
C      dTstab = potential temp. increment for stability function derivative
C      FSTAB  = Amplitude of stability correction (fraction)
C      HDRAG  = Height scale for orographic correction
C      FHDRAG = Amplitude of orographic correction (fraction)
      COMMON /SFLCON/ FWIND0, FTEMP0, FHUM0,
     &                CDL, CDS, CHL, CHS, VGUST, CTDAY,
     &                DTHETA, dTstab, FSTAB, HDRAG, FHDRAG
      _RL FWIND0, FTEMP0, FHUM0
      _RL CDL, CDS, CHL, CHS, VGUST, CTDAY
      _RL DTHETA, dTstab, FSTAB, HDRAG, FHDRAG

C--   COMMON /SFLFIX/: Time-invariant fields (initial. in SFLSET)
c     COMMON /SFLFIX/ FOROG
c     _RL FOROG(NGP)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
