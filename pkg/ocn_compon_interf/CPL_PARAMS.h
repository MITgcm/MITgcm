C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/CPL_PARAMS.h,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#ifdef COMPONENT_MODULE
C     *==========================================================*
C     | CPL_PARAMS.h
C     | o Header file for Coupling component interface
C     *==========================================================*
C     |   this version is specific to 1 component (ocean)
C     *==========================================================*

C--   COMMON /CPL_OCN_PAR_L/: logical parameters
C     useImportHFlx :: True => use the Imported HeatFlux from couler
C     useImportFW   :: True => use the Imported Fresh Water flux fr cpl
C     useImportTau  :: True => use the Imported Wind-Stress from couler
      COMMON /LAND_PAR_L/
     &  useImportHFlx, useImportFW, useImportTau
      LOGICAL useImportHFlx
      LOGICAL useImportFW
      LOGICAL useImportTau

C--   COMMON /CPL_OCN_PAR_C/: Character valued parameters
c     CHARACTER*(MAX_LEN_FNAM) cpl_ocnFile

C--   COMMON /CPL_OCN_PAR_R/: real-type parameters
C     cpl_taveFreq   :: Frequency^-1 for time-Aver. output (s)
      COMMON /CPL_OCN_PAR_R/ 
     &    cpl_taveFreq
      _RL cpl_taveFreq

#endif /* COMPONENT_MODULE */ 
