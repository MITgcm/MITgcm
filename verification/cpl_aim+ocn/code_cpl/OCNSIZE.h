C $Header: /u/gcmpack/MITgcm/verification/cpl_aim+ocn/code_cpl/OCNSIZE.h,v 1.1 2003/12/15 21:05:19 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | OCN_SIZE.h Declare size of underlying computational grid |
C     |            for ocean component.                          |
C     \==========================================================/
C     Nx_ocn  - No. points in X for the total domain.
C     Ny_ocn  - No. points in Y for the total domain.
      INTEGER Nx_ocn
      INTEGER Ny_ocn
      PARAMETER (
     &           Nx_ocn  = 192,
     &           Ny_ocn  =  32)
