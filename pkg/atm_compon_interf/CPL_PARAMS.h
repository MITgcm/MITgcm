C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/CPL_PARAMS.h,v 1.6 2013/09/24 23:13:54 jmc Exp $
C $Name:  $

#ifdef COMPONENT_MODULE
C     *==========================================================*
C     | CPL_PARAMS.h
C     | o Header file for Coupling component interface
C     *==========================================================*
C     |   this version is specific to 1 component (atmos)
C     *==========================================================*

C--   COMMON /CPL_ATM_PAR_L/: logical parameters
C     cpl_oldPickup :: restart from an old pickup (= until checkpoint 59h)
C     useImportMxlD :: True => use Imported Mix.Layer Detph from coupler
C     useImportSST  :: True => use the Imported SST from coupler
C     useImportSSS  :: True => use the Imported SSS from coupler
C     useImportVsq  :: True => use the Imported Surf. velocity^2
C     useImportFlxCO2 :: True => use the Imported air-sea CO2 fluxes from coupler
      COMMON /CPL_ATM_PAR_L/
     &  cpl_oldPickup,
     &  useImportMxlD, useImportSST, useImportSSS,
     &  useImportVsq, useImportFlxCO2

      LOGICAL cpl_oldPickup
      LOGICAL useImportMxlD, useImportSST, useImportSSS
      LOGICAL useImportVsq, useImportFlxCO2

C--   COMMON /CPL_ATM_PAR_I/: Integer valued parameters
C     cplSendFrq_iter :: send data to coupler every "cplSendFrq" iter
C     maxNumberPrint  :: max number of printed Export/Import messages
C     countPrtExp     :: counter for printed Export message
C     countPrtImp     :: counter for printed Import message
      COMMON /CPL_ATM_PAR_I/
     &  cplSendFrq_iter,
     &  maxNumberPrint, countPrtExp, countPrtImp
      INTEGER cplSendFrq_iter
      INTEGER maxNumberPrint
      INTEGER countPrtExp
      INTEGER countPrtImp

C--   COMMON /CPL_ATM_PAR_C/: Character valued parameters
c     CHARACTER*(MAX_LEN_FNAM) cpl_atmFile

C--   COMMON /CPL_ATM_PAR_R/: real-type parameters
C     cpl_atmSendFrq  :: Frequency^-1 for sending data to coupler (s)
c     COMMON /CPL_ATM_PAR_R/
c    &    cpl_atmSendFrq

#endif /* COMPONENT_MODULE */
