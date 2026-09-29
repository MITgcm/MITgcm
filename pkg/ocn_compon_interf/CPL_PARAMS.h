#ifdef COMPONENT_MODULE
C     *==========================================================*
C     | CPL_PARAMS.h
C     | o Header file for Coupling component interface
C     *==========================================================*
C     |   this version is specific to 1 component (ocean)
C     *==========================================================*

C--   COMMON /CPL_OCN_SWITCH/: from coupler, control switch
C                              of optionnally exchanged fields;
C     cpl_exchange_RunOff :: controls exchange of RunOff fields
C     cpl_exchange1W_sIce :: controls 1-way exchange of seaice (step fwd in ATM)
C     cpl_exchange2W_sIce :: controls 2-way exchange of ThSIce variables
C     cpl_exchange_SaltPl :: controls exchange of Salt-Plume fields
C     cpl_exchange_DIC    :: controls exchange of DIC variables
      COMMON /CPL_OCN_SWITCH/
     &     cpl_exchange_RunOff,
     &     cpl_exchange1W_sIce, cpl_exchange2W_sIce,
     &     cpl_exchange_SaltPl,
     &     cpl_exchange_DIC
      INTEGER cpl_exchange_RunOff
      INTEGER cpl_exchange1W_sIce
      INTEGER cpl_exchange2W_sIce
      INTEGER cpl_exchange_SaltPl
      INTEGER cpl_exchange_DIC

C--   COMMON /CPL_OCN_PAR_L/: logical parameters
C     ocn_cplSequential  :: use Sequential Coupling (instead of Synchronous)
C     ocn_cplExch_RunOff :: exchange RunOff     fields with coupler
C     ocn_cplExch1W_sIce :: 1-way exchange of seaice fields with coupler
C     ocn_cplExch2W_sIce :: 2-way exchange of ThSIce fields with coupler
C     ocn_cplExch_SaltPl :: exchange Salt-Plume fields with coupler
C     ocn_cplExch_DIC    :: exchange DIC        fields with coupler
C     useImportHFlx :: True => use the Imported HeatFlux from couler
C     useImportFW   :: True => use the Imported Fresh Water flux fr cpl
C     useImportTau  :: True => use the Imported Wind-Stress from couler
C     useImportSLP  :: True => use the Imported Sea-level Pressure
C     useImportRunOff :: True => use the Imported RunOff flux from coupler
C     useImportSIce   :: True => use the Imported Sea-Ice mass as ice-loading
C     useImportThSIce :: True => use the Imported thSIce state var from coupler
C     useImportSltPlm :: True => use the Imported Salt-Plume flux from coupler
C     useImportFice   :: True => use the Imported Seaice fraction (DIC-only)
C     useImportCO2    :: True => use the Imported atmos. CO2 from coupler
C     useImportWSpd   :: True => use the Imported surf. Wind speed from coupler

      COMMON /CPL_OCN_PAR_L/
     &  ocn_cplSequential,
     &  ocn_cplExch_RunOff,
     &  ocn_cplExch1W_sIce, ocn_cplExch2W_sIce, ocn_cplExch_SaltPl,
     &  ocn_cplExch_DIC,
     &  useImportHFlx, useImportFW, useImportTau,
     &  useImportSLP,  useImportRunOff,
     &  useImportSIce, useImportThSIce, useImportSltPlm,
     &  useImportFice, useImportCO2, useImportWSpd,
     &  cpl_snapshot_mdsio, cpl_snapshot_mnc

      LOGICAL ocn_cplSequential
      LOGICAL ocn_cplExch_RunOff
      LOGICAL ocn_cplExch1W_sIce
      LOGICAL ocn_cplExch2W_sIce
      LOGICAL ocn_cplExch_SaltPl
      LOGICAL ocn_cplExch_DIC
      LOGICAL useImportHFlx
      LOGICAL useImportFW
      LOGICAL useImportTau
      LOGICAL useImportSLP
      LOGICAL useImportRunOff
      LOGICAL useImportSIce
      LOGICAL useImportThSIce
      LOGICAL useImportSltPlm
      LOGICAL useImportFice
      LOGICAL useImportCO2
      LOGICAL useImportWSpd
      LOGICAL cpl_snapshot_mdsio, cpl_snapshot_mnc

C--   COMMON /CPL_OCN_PAR_I/: Integer valued parameters
C     maxNumberPrint :: max number of printed Export/Import messages
C     countPrtExp    :: counter for printed Export message
C     countPrtImp    :: counter for printed Import message
C     cplErrorCount  :: counter for errors in coupling config
      COMMON /CPL_OCN_PAR_I/
     &  cplErrorCount
      INTEGER cplErrorCount

C--   COMMON /CPL_OCN_PAR_C/: Character valued parameters
c     CHARACTER*(MAX_LEN_FNAM) cpl_ocnFile

#endif /* COMPONENT_MODULE */
