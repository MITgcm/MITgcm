C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/CPL_PARAMS.h,v 1.1 2006/06/15 23:05:25 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | CPL_PARAMS.h
C     |  - parameter for the Coupler, holds in common block
C     *==========================================================*
      
C--   COMMON /CPL_PAR_I/: Integer parameters
C     nCouplingSteps   :: Number of coupling steps
      COMMON /CPL_PAR_I/
     &     nCouplingSteps
      INTEGER nCouplingSteps
