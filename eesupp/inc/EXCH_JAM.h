C $Header: /u/gcmpack/MITgcm/eesupp/inc/Attic/EXCH_JAM.h,v 1.3 2001/02/04 14:38:41 cnh Exp $
C $Name:  $
      COMMON /EXCH_BUF/ exchBuf1, exchBuf2
      Real*8 exchBuf1(((sNx+2*OLx)*OLy)*Nr)
      Real*8 exchBuf2(((sNx+2*OLx)*OLy)*Nr)
