module OAD_rev

  implicit none

  private
  public :: modeType, our_rev_mode, OAD_revInit, &
OAD_revPlain, OAD_revTape, OAD_revAdjoint, & 
OAD_revStorePlain, OAD_revRestoreTape, &
our_indent, splitPrint, oad_dump_revmod

  type modeType
     logical :: arg_store=.FALSE.
     logical :: arg_restore=.FALSE.
     logical :: res_store=.FALSE.
     logical :: res_restore=.FALSE.
     logical :: plain=.FALSE.
     logical :: tape=.FALSE.
     logical :: adjoint=.FALSE.
     logical :: switchedToCheckpoint=.FALSE.
     logical :: strictAnonymous=.TRUE.
  end type modeType

  type(modeType), save :: our_rev_mode

  interface OAD_revInit
     module procedure init
  end interface
  
  interface OAD_revPlain
     module procedure plain
  end interface
  
  interface OAD_revTape
     module procedure tape
  end interface
  
  interface OAD_revAdjoint
     module procedure adjoint
  end interface
  
  interface OAD_revStorePlain
     module procedure storePlain
  end interface
  
  interface OAD_revRestoreTape
     module procedure restoreTape
  end interface

  integer, save:: our_indent=0
  logical, save:: splitPrint=.false.

  interface oad_dump_revmod
     module procedure dump_revmod
  end interface

contains

  subroutine init()
    our_rev_mode%arg_store=.FALSE.
    our_rev_mode%arg_restore=.FALSE.
    our_rev_mode%res_store=.FALSE.
    our_rev_mode%res_restore=.FALSE.
    our_rev_mode%plain=.TRUE.
    our_rev_mode%tape=.FALSE.
    our_rev_mode%adjoint=.FALSE.
  end subroutine 

  subroutine plain()
    our_rev_mode%arg_store=.FALSE.
    our_rev_mode%arg_restore=.FALSE.
    our_rev_mode%res_store=.FALSE.
    our_rev_mode%res_restore=.FALSE.
    our_rev_mode%plain=.TRUE.
    our_rev_mode%tape=.FALSE.
    our_rev_mode%adjoint=.FALSE.
  end subroutine 

  subroutine tape()
    our_rev_mode%arg_store=.FALSE.
    our_rev_mode%arg_restore=.FALSE.
    our_rev_mode%res_store=.FALSE.
    our_rev_mode%res_restore=.FALSE.
    our_rev_mode%plain=.FALSE.
    our_rev_mode%tape=.TRUE.
    our_rev_mode%adjoint=.FALSE.
  end subroutine 

  subroutine adjoint()
    our_rev_mode%arg_store=.FALSE.
    our_rev_mode%arg_restore=.FALSE.
    our_rev_mode%res_store=.FALSE.
    our_rev_mode%res_restore=.FALSE.
    our_rev_mode%plain=.FALSE.
    our_rev_mode%tape=.FALSE.
    our_rev_mode%adjoint=.TRUE.
  end subroutine 

  subroutine storePlain()
    our_rev_mode%arg_store=.TRUE.
    our_rev_mode%arg_restore=.FALSE.
    our_rev_mode%res_store=.FALSE.
    our_rev_mode%res_restore=.FALSE.
    our_rev_mode%plain=.TRUE.
    our_rev_mode%tape=.FALSE.
    our_rev_mode%adjoint=.FALSE.
  end subroutine 

  subroutine restoreTape()
    our_rev_mode%arg_store=.FALSE.
    our_rev_mode%arg_restore=.TRUE.
    our_rev_mode%res_store=.FALSE.
    our_rev_mode%res_restore=.FALSE.
    our_rev_mode%plain=.FALSE.
    our_rev_mode%tape=.TRUE.
    our_rev_mode%adjoint=.FALSE.
  end subroutine 

  subroutine dump_revmod()
    write(*,'(A,L,A,L,A,L,A,L,A,L,A,L)', ADVANCE='NO') & 
         ' As:',our_rev_mode%arg_store, &
         ' Ar:',our_rev_mode%arg_restore, &
         ' Pl:',our_rev_mode%plain, &
         ' Ta:',our_rev_mode%tape, &
         ' Ad:',our_rev_mode%adjoint, &
         ' Sw:',our_rev_mode%switchedToCheckpoint
  end subroutine dump_revmod

end module OAD_rev
