C $Header: /u/gcmpack/MITgcm/eesupp/inc/Attic/EXCH_JAM.h,v 1.4 2001/09/21 03:54:35 cnh Exp $
C $Name:  $
CBOP
C     !ROUTINE: EXCH_JAM.h
C     !INTERFACE:
C     include "EXCH_JAM.h"
C     !DESCRIPTION:
C     *=====================================================================================*
C     | Global data structures used by JAM exchange routines.                                
C     *=====================================================================================*
CEOP
      COMMON /EXCH_BUF/ exchBuf1, exchBuf2
      Real*8 exchBuf1(((sNx+2*OLx)*OLy)*Nr)
      Real*8 exchBuf2(((sNx+2*OLx)*OLy)*Nr)
