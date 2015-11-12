C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/CPL_PARAMS.h,v 1.7 2015/11/12 00:55:18 jmc Exp $
C $Name:  $

#ifdef COMPONENT_MODULE
C     *==========================================================*
C     | CPL_PARAMS.h
C     | o Header file for Coupling component interface
C     *==========================================================*
C     |   this version is specific to 1 component (ocean)
C     *==========================================================*

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
C     useImportSIce :: True => use the Imported Sea-Ice mass as ice-loading
C     useImportFice :: True => use the Imported Seaice fraction fr cpl
C     useImportCO2  :: True => use the Imported atmos. CO2 from coupler
C     useImportWSpd :: True => use the Imported surface Wind speed fr cpl

      COMMON /CPL_OCN_PAR_L/
     &  ocn_cplSequential,
     &  ocn_cplExch_RunOff,
     &  ocn_cplExch1W_sIce, ocn_cplExch2W_sIce, ocn_cplExch_SaltPl,
     &  ocn_cplExch_DIC,
     &  useImportHFlx, useImportFW, useImportTau,
     &  useImportSLP, useImportSIce, useImportFIce,
     &  useImportCO2, useImportWSpd,
     &  cpl_snapshot_mdsio, cpl_snapshot_mnc,
     &  cpl_timeave_mdsio, cpl_timeave_mnc

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
      LOGICAL useImportSIce
      LOGICAL useImportFIce
      LOGICAL useImportCO2
      LOGICAL useImportWSpd
      LOGICAL
     &     cpl_snapshot_mdsio, cpl_snapshot_mnc,
     &     cpl_timeave_mdsio, cpl_timeave_mnc

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

C--   COMMON /CPL_OCN_PAR_R/: real-type parameters
C     cpl_taveFreq   :: Frequency^-1 for time-Aver. output (s)
      COMMON /CPL_OCN_PAR_R/
     &    cpl_taveFreq
      _RL cpl_taveFreq

#endif /* COMPONENT_MODULE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
