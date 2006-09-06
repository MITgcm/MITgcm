C $Header: /u/gcmpack/MITgcm/pkg/atm2d/OCNSIZE.h,v 1.1 2006/09/06 15:32:40 jscott Exp $
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
     &           Nx_ocn  = 90,
     &           Ny_ocn  = 44)

