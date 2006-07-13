module OpenAD_dct
  implicit none

  private
  public :: dct_node, dct

  type dct_node
    integer :: dble_tape_base=1
    integer :: dble_argcp_base=1
    integer :: dble_rescp_base=1
  end type dct_node

  integer, parameter :: max_nr_calls=10
  type(dct_node), dimension(max_nr_calls), save, target :: dct

end module OpenAD_dct
