C $Header: /u/gcmpack/MITgcm/verification/cpl_aim+ocn/code_cpl/ATMSIZE.h,v 1.1 2003/12/15 21:05:19 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | ATMSIZE.h  Declare size of underlying computational grid |
C     |            for atmosphere component.                     |
C     \==========================================================/
C     Nx_atm  - No. points in X for the total domain.
C     Ny_atm  - No. points in Y for the total domain.
      INTEGER Nx_atm
      INTEGER Ny_atm
      PARAMETER (
     &           Nx_atm  = 192,
     &           Ny_atm  =  32)
