C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_vdicon.h,v 1.1 2002/11/22 17:16:07 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C--   COMMON /VDICON/: Constants for vertical diffusion and shallow convection
C                      (initial. in INPHYS)
C      TRSHC  = relaxation time (in hours) for shallow convection
C      TRVDI  = relaxation time (in hours) for moisture diffusion
C      TRVDS  = relaxation time (in hours) for super-adiab. conditions
C      RHGRAD = maximum gradient of relative humidity (d_RH/d_sigma)
C      SEGRAD = minimum gradient of dry static energy (d_DSE/d_phi)
      COMMON /VDICON/ TRSHC, TRVDI, TRVDS, RHGRAD, SEGRAD
      _RL             TRSHC, TRVDI, TRVDS, RHGRAD, SEGRAD

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */ 
