C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/com_cnvcon.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $
C $Namer: $

#ifdef ALLOW_AIM

C--   COMMON /CNVCON/: Convection constants (initial. in INPHYS)
C      PSMIN  = minimum (norm.) sfc. pressure for the occurrence of convection
C      TRCNV  = time of relaxation (in hours) towards reference state
C      QBL    = specific hum. threshold in the boundary layer
C      RHBL   = relative hum. threshold in the boundary layer
C      RHIL   = rel. hum. threshold in intermed. layers for secondary mass flux
C      ENTMAX = max. entrainment as a fraction of cloud-base mass flux
C      SMF    = ratio between secondary and primary mass flux at cloud-base
 
      COMMON /CNVCON/ PSMIN, TRCNV, QBL, RHBL, RHIL, ENTMAX, SMF
      _RL             PSMIN, TRCNV, QBL, RHBL, RHIL, ENTMAX, SMF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */
