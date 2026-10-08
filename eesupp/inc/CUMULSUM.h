!BOP
! !ROUTINE: CUMULSUM.h
! !INTERFACE:
! include "CUMULSUM.h"
! !DESCRIPTION:
! *==========================================================*
! | CUMULSUM.h
! | o Globals used by Fortran CUMULSUM\_TILE routine.
! *==========================================================*
! *==========================================================*
!EOP

! shareBufCS2_R8 :: holds tile increment along X direction (1rst index=1)
!                   and along Y direction (1rst index=2) (CUMULSUM input)
! shareBufCS1_R8 :: holds tile cumulated sum at tile origin (i,j)=1,1
      COMMON  / CUMULSUM_R8 /  shareBufCS1_R8, shareBufCS2_R8
      Real(kind=8) :: shareBufCS1_R8  (nSx,nSy)
      Real(kind=8) :: shareBufCS2_R8(2,nSx,nSy)

!EH3 ;;; Local Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
