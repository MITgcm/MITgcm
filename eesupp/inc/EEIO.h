C $Id: EEIO.h,v 1.1.1.1 1998/04/22 19:15:30 cnh Exp $
C
C     /==========================================================\
C     | EEIO.h                                                   |
C     |==========================================================|
C     | Support data structures for the MITgcm UV "execution     |
C     | environment" IO code.                                    |
C     \==========================================================/


C--   COMMON /EEIO_R/ IO supporting real arrays
C     tmpXY_R8 - XY Real*8 IO buffer.
C     tmpXY_R4 - XY Real*4 IO buffer.
      COMMON /EESUPP_IO_R/ IO_tmpXY_R8, IO_tmpXY_R4
      REAL IO_tmpXY_R8(Nx,Ny)
      real IO_tmpXY_R4(Nx,Ny)

