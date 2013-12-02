C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/CPL_PARAMS.h,v 1.4 2013/12/02 22:03:08 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | CPL_PARAMS.h
C     |  - parameter for the Coupler, holds in common block
C     *==========================================================*

C--   COMMON /CPL_PAR_I/ Integer parameters
C     runOffMapSize    :: Nunber of connected grid points in RunOff-Map
C     nCouplingSteps   :: Number of coupling steps
C     nSteps_atm       :: Number of coupling steps done by ATM
C     nSteps_ocn       :: Number of coupling steps done by OCN
      COMMON /CPL_PAR_I/
     &     runOffMapSize,
     &     nCouplingSteps,
     &     nSteps_atm, nSteps_ocn
      INTEGER runOffMapSize
      INTEGER nCouplingSteps
      INTEGER nSteps_atm, nSteps_ocn

C--   COMMON /CPL_PAR_C/ Character parameters
C     runOffMapFile    :: Input file for setting runoffmap
      COMMON /CPL_PAR_C/ runOffMapFile
      CHARACTER*80 runOffMapFile

