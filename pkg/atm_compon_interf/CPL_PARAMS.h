C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/CPL_PARAMS.h,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $

#ifdef COMPONENT_MODULE
C     *==========================================================*
C     | CPL_PARAMS.h
C     | o Header file for Coupling component interface
C     *==========================================================*
C     |   this version is specific to 1 component (atmos)
C     *==========================================================*

C--   COMMON /CPL_ATM_PAR_L/: logical parameters
C     useImportSST   :: True => use the Imported SST from couler
      COMMON /LAND_PAR_L/
     &  useImportSST
      LOGICAL useImportSST

C--   COMMON /CPL_ATM_PAR_I/: Integer valued parameters
C     cplSendFrq_iter :: send data to coupler every "cplSendFrq" iter
      COMMON /CPL_ATM_PAR_I/
     &  cplSendFrq_iter
      INTEGER cplSendFrq_iter

C--   COMMON /CPL_ATM_PAR_C/: Character valued parameters
c     CHARACTER*(MAX_LEN_FNAM) cpl_atmFile

C--   COMMON /CPL_ATM_PAR_R/: real-type parameters
C     cpl_atmSendFrq  :: Frequency^-1 for sending data to coupler (s)
c     COMMON /CPL_ATM_PAR_R/ 
c    &    cpl_atmSendFrq

#endif /* COMPONENT_MODULE */ 
