C $Header: /u/gcmpack/MITgcm/eesupp/inc/Attic/EEIO.h,v 1.5 2001/09/21 03:54:35 cnh Exp $
C $Name:  $

#ifdef USE_EEIO
CBOP
C     !ROUTINE: EEIO.h
C     !INTERFACE:
C     include "EEIO.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EEIO.h                                                    
C     *==========================================================*
C     | Support data structures for the MITgcm UV "execution      
C     | environment" IO code.                                     
C     *==========================================================*
CEOP

C--   COMMON /EEIO_R/ IO supporting real arrays
C     tmpXY_R8 - XY Real*8 IO buffer.
C     tmpXY_R4 - XY Real*4 IO buffer.
      COMMON /EESUPP_IO_R/ IO_tmpXY_R8, IO_tmpXY_R4
      Real*8 IO_tmpXY_R8(Nx,Ny)
      Real*4 IO_tmpXY_R4(Nx,Ny)

#endif
