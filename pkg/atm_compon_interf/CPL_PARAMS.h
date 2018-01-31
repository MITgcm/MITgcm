#ifdef COMPONENT_MODULE
C     *==========================================================*
C     | CPL_PARAMS.h
C     | o Header file for Coupling component interface
C     *==========================================================*
C     |   this version is specific to 1 component (atmos)
C     *==========================================================*

C--   COMMON /CPL_ATM_SWITCH/: from coupler, control switch
C                              of optionnally exchanged fields;
C     cpl_exchange_RunOff :: controls exchange of RunOff fields
C     cpl_exchange1W_sIce :: controls 1-way exchange of seaice (step fwd in ATM)
C     cpl_exchange2W_sIce :: controls 2-way exchange of ThSIce variables
C     cpl_exchange_SaltPl :: controls exchange of Salt-Plume fields
C     cpl_exchange_DIC    :: controls exchange of DIC variables
      COMMON /CPL_ATM_SWITCH/
     &     cpl_exchange_RunOff,
     &     cpl_exchange1W_sIce, cpl_exchange2W_sIce,
     &     cpl_exchange_SaltPl,
     &     cpl_exchange_DIC
      INTEGER cpl_exchange_RunOff
      INTEGER cpl_exchange1W_sIce
      INTEGER cpl_exchange2W_sIce
      INTEGER cpl_exchange_SaltPl
      INTEGER cpl_exchange_DIC

C--   COMMON /CPL_ATM_PAR_L/: logical parameters
C     atm_cplSequential  :: use Sequential Coupling (instead of Synchronous)
C     atm_cplExch_RunOff :: exchange RunOff     fields with coupler
C     atm_cplExch1W_sIce :: 1-way exchange of seaice fields with coupler
C     atm_cplExch2W_sIce :: 2-way exchange of ThSIce fields with coupler
C     atm_cplExch_SaltPl :: exchange Salt-Plume fields with coupler
C     atm_cplExch_DIC    :: exchange DIC        fields with coupler
C     cpl_oldPickup :: restart from an old pickup (= until checkpoint 59h)
C     useImportMxlD :: True => use Imported Mix.Layer Detph from coupler
C     useImportSST  :: True => use the Imported SST from coupler
C     useImportSSS  :: True => use the Imported SSS from coupler
C     useImportVsq  :: True => use the Imported Surf. velocity^2
C     useImportThSIce :: True => use the Imported thSIce state var from coupler
C     useImportFlxCO2 :: True => use the Imported air-sea CO2 flux from coupler
      COMMON /CPL_ATM_PAR_L/
     &  atm_cplSequential,
     &  atm_cplExch_RunOff,
     &  atm_cplExch1W_sIce, atm_cplExch2W_sIce, atm_cplExch_SaltPl,
     &  atm_cplExch_DIC,
     &  cpl_oldPickup,
     &  useImportMxlD, useImportSST, useImportSSS,
     &  useImportVsq, useImportThSIce, useImportFlxCO2

      LOGICAL atm_cplSequential
      LOGICAL atm_cplExch_RunOff
      LOGICAL atm_cplExch1W_sIce
      LOGICAL atm_cplExch2W_sIce
      LOGICAL atm_cplExch_SaltPl
      LOGICAL atm_cplExch_DIC
      LOGICAL cpl_oldPickup
      LOGICAL useImportMxlD, useImportSST, useImportSSS
      LOGICAL useImportVsq, useImportThSIce, useImportFlxCO2

C--   COMMON /CPL_ATM_PAR_I/: Integer valued parameters
C     cplSendFrq_iter :: send data to coupler every "cplSendFrq" iter
C     maxNumberPrint  :: max number of printed Export/Import messages
C     countPrtExp     :: counter for printed Export message
C     countPrtImp     :: counter for printed Import message
C     cplErrorCount   :: counter for errors in coupling config
      COMMON /CPL_ATM_PAR_I/
     &  cplSendFrq_iter,
     &  maxNumberPrint, countPrtExp, countPrtImp,
     &  cplErrorCount
      INTEGER cplSendFrq_iter
      INTEGER maxNumberPrint
      INTEGER countPrtExp
      INTEGER countPrtImp
      INTEGER cplErrorCount

C--   COMMON /CPL_ATM_PAR_C/: Character valued parameters
c     CHARACTER*(MAX_LEN_FNAM) cpl_atmFile

C--   COMMON /CPL_ATM_PAR_R/: real-type parameters
C     cpl_atmSendFrq  :: Frequency^-1 for sending data to coupler (s)
c     COMMON /CPL_ATM_PAR_R/
c    &    cpl_atmSendFrq

#endif /* COMPONENT_MODULE */
