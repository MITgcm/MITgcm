module OpenAD_checkpoints

  implicit none

  private

  public :: cp_double, cp_double_tmp, &
&           cp_double_pointer, cp_double_size, &
&           cp_integer, cp_integer_tmp, &
&           cp_integer_pointer, cp_integer_size, &
&           cp_boolean, cp_boolean_tmp, &
&           cp_boolean_pointer, cp_boolean_size, &
&           cp_string, cp_string_tmp, &
&           cp_string_pointer, cp_string_size, &
&           cp_init

  double precision, dimension(:), allocatable :: cp_double, cp_double_tmp
  integer :: cp_double_pointer=0, cp_double_size=0

  integer, dimension(:), allocatable :: cp_integer, cp_integer_tmp
  integer :: cp_integer_pointer=0, cp_integer_size=0

  logical, dimension(:), allocatable :: cp_boolean, cp_boolean_tmp
  integer :: cp_boolean_pointer=0, cp_boolean_size=0

  character*(80), dimension(:), allocatable :: cp_string, cp_string_tmp
  integer :: cp_string_pointer=0, cp_string_size=0

  interface cp_init
    module procedure init
  end interface cp_init

contains

  subroutine init
    cp_double_pointer=1
    if (allocated(cp_double)) then 
       deallocate(cp_double)
    end if
    cp_double_size=1048576
    allocate(cp_double(cp_double_size))
    cp_integer_pointer=1
    if (allocated(cp_integer)) then 
       deallocate(cp_integer)
    end if
    cp_integer_size=1048576
    allocate(cp_integer(cp_integer_size))
    cp_boolean_pointer=1
    if (allocated(cp_boolean)) then 
       deallocate(cp_boolean)
    end if
    cp_boolean_size=1024
    allocate(cp_boolean(cp_boolean_size))
    cp_string_pointer=1
    if (allocated(cp_string)) then 
       deallocate(cp_string)
    end if
    cp_string_size=1024
    allocate(cp_string(cp_string_size))
  end subroutine init

end module OpenAD_checkpoints
