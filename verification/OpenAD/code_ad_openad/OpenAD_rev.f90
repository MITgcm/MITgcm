module OpenAD_rev

implicit none

private
public :: modeType, our_rev_mode, our_indent
!, forward_mode, &
!& forward_arg_store_mode, reverse_mode, adjoint_mode, taping_mode, restore_mode

type modeType
  logical :: arg_store=.FALSE.
  logical :: arg_restore=.FALSE.
  logical :: res_store=.FALSE.
  logical :: res_restore=.FALSE.
  logical :: plain=.FALSE.
  logical :: tape=.FALSE.
  logical :: adjoint=.FALSE.
end type modeType

type(modeType), save :: our_rev_mode
integer, save:: our_indent=0

! interface forward_mode
!   module procedure forward_mode_i
! end interface
! 
! interface forward_arg_store_mode
!   module procedure forward_arg_store_mode_i
! end interface
! 
! interface taping_mode
!   module procedure taping_mode_i
! end interface
! 
! interface adjoint_mode
!   module procedure adjoint_mode_i
! end interface
! 
! interface reverse_mode
!   module procedure reverse_mode_i
! end interface
! 
! interface restore_mode
!   module procedure restore_mode_i
! end interface
! 
! contains
! 
! subroutine forward_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.TRUE.
!   our_rev_mode%tape=.FALSE.
!   our_rev_mode%adjoint=.FALSE.
! end subroutine 
! 
! subroutine forward_arg_store_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.TRUE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.TRUE.
!   our_rev_mode%tape=.FALSE.
!   our_rev_mode%adjoint=.FALSE.
! end subroutine 
! 
! subroutine taping_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.FALSE.
!   our_rev_mode%tape=.TRUE.
!   our_rev_mode%adjoint=.FALSE.
! end subroutine 
! 
! subroutine adjoint_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.FALSE.
!   our_rev_mode%tape=.FALSE.
!   our_rev_mode%adjoint=.TRUE.
! end subroutine 
! 
! subroutine reverse_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.TRUE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.FALSE.
!   our_rev_mode%tape=.TRUE.
!   our_rev_mode%adjoint=.TRUE.
! end subroutine 
! 
! subroutine restore_mode_i()
!   our_rev_mode=our_orig_mode
! end subroutine 
	
end module OpenAD_rev
