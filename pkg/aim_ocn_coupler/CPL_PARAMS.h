C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/CPL_PARAMS.h,v 1.1 2003/12/15 02:28:00 jmc Exp $
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
