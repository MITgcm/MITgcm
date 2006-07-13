module OpenAD_tape
  implicit none

  private
  public :: double_tape, double_tmp_tape, &
&           double_tape_pointer, double_tape_size, &
&           integer_tape, integer_tmp_tape, &
&           integer_tape_pointer, integer_tape_size, &
&           tape_init

  double precision, dimension(:), allocatable :: double_tape, double_tmp_tape
  integer, dimension(:), allocatable :: integer_tape, integer_tmp_tape
  integer :: double_tape_pointer=0, integer_tape_pointer=0
  integer :: double_tape_size=0, integer_tape_size=0

  interface tape_init
    module procedure init
  end interface tape_init

contains

  subroutine init
    double_tape_pointer=1
    if (allocated(double_tape)) then 
       deallocate(double_tape)
    end if
    double_tape_size=1048576
    allocate(double_tape(double_tape_size))
    integer_tape_pointer=1
    if (allocated(integer_tape)) then 
       deallocate(integer_tape)
    end if
    integer_tape_size=1048576
    allocate(integer_tape(integer_tape_size))
  end subroutine init

end module OpenAD_tape
