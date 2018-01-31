C     *==========================================================*
C     | CPL_PARAMS.h
C     |  - parameter for the Coupler, holds in common block
C     *==========================================================*

C--   COMMON /CPL_PAR_I/ Integer parameters
C     cpl_sequential   :: =0/1 : selects Synchronous/Sequential Coupling
C-- cpl_exchange_[xxx] :: controls exchange of [xxx] fields with OCN & ATM comp:
C                      ::  =0 : none ; =1,3 : exch [xxx] fields with OCN comp.
C                      ::              =2,3 : exch [xxx] fileds with ATM comp.
C     cpl_exchange_RunOff :: controls exchange of RunOff fields
C     cpl_exchange1W_sIce :: controls 1-way exchange of seaice (step fwd in ATM)
C     cpl_exchange2W_sIce :: controls 2-way exchange of ThSIce variables
C     cpl_exchange_SaltPl :: controls exchange of Salt-Plume fields
C     cpl_exchange_DIC    :: controls exchange of DIC variables
C--
C     runOffMapSize    :: Nunber of connected grid points in RunOff-Map
C     nCouplingSteps   :: Number of coupling steps
C     nSteps_atm       :: Number of coupling steps done by ATM
C     nSteps_ocn       :: Number of coupling steps done by OCN
C     cplErrorCount    :: counter for errors in coupling config
      COMMON /CPL_PAR_I/
     &     cpl_sequential,
     &     cpl_exchange_RunOff,
     &     cpl_exchange1W_sIce, cpl_exchange2W_sIce,
     &     cpl_exchange_SaltPl,
     &     cpl_exchange_DIC,
     &     runOffMapSize,
     &     nCouplingSteps,
     &     nSteps_atm, nSteps_ocn,
     &     cplErrorCount
      INTEGER cpl_sequential
      INTEGER cpl_exchange_RunOff
      INTEGER cpl_exchange1W_sIce
      INTEGER cpl_exchange2W_sIce
      INTEGER cpl_exchange_SaltPl
      INTEGER cpl_exchange_DIC
      INTEGER runOffMapSize
      INTEGER nCouplingSteps
      INTEGER nSteps_atm, nSteps_ocn
      INTEGER cplErrorCount

C--   COMMON /CPL_PAR_C/ Character parameters
C     runOffMapFile    :: Input file for setting runoffmap
      COMMON /CPL_PAR_C/ runOffMapFile
      CHARACTER*80 runOffMapFile

