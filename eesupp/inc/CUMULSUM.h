C $Header: /u/gcmpack/MITgcm/eesupp/inc/CUMULSUM.h,v 1.1 2011/07/09 21:56:55 jmc Exp $
C $Name:  $
CBOP
C     !ROUTINE: CUMULSUM.h
C     !INTERFACE:
C     include "CUMULSUM.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | CUMULSUM.h
C     | o Globals used by Fortran CUMULSUM\_TILE routine.
C     *==========================================================*
C     *==========================================================*
CEOP

C     shareBufCS2_R8 :: holds tile increment along X direction (1rst index=1)
C                       and along Y direction (1rst index=2) (CUMULSUM input)
C     shareBufCS1_R8 :: holds tile cumulated sum at tile origin (i,j)=1,1
      COMMON  / CUMULSUM_R8 /  shareBufCS1_R8, shareBufCS2_R8
      Real*8  shareBufCS1_R8  (nSx,nSy)
      Real*8  shareBufCS2_R8(2,nSx,nSy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
