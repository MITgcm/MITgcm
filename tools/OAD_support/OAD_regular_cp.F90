module OAD_regular_cp

  implicit none

  private :: cp_file_number, cp_open

  public :: cp_io_unit, cp_init, cp_write_open, cp_read_open, cp_close, cp_fNumber
  public :: cp_open_state,  cp_close_state, cp_read_state, cp_write_state

  integer :: cp_file_number, cp_io_unit, cp_state_unit

  interface cp_init
     module procedure init_i
  end interface

  interface cp_open
     module procedure open_i
  end interface

  interface cp_write_open
     module procedure write_open_i
     module procedure write_openX_i
  end interface

  interface cp_read_open
     module procedure read_open_i
     module procedure read_openX_i
  end interface

  interface cp_close
     module procedure close_i
  end interface

  interface cp_findunit
     module procedure findunit_i
  end interface
  
  interface cp_open_state
     module procedure open_state_i
  end interface

  interface cp_close_state
     module procedure close_state_i
  end interface

  interface cp_read_state
     module procedure read_state_i
  end interface

  interface cp_write_state
     module procedure write_state_i
  end interface

contains

  subroutine init_i
    implicit none
    cp_file_number=1
  end subroutine

  subroutine write_open_i()
    implicit none
    call cp_open()
!    print *, 'writing ', cp_file_number
    cp_file_number=cp_file_number+1
  end subroutine 

  subroutine write_openX_i(X)
    implicit none
    integer X
    cp_file_number=X
!    print *, 'writing ', cp_file_number
    call cp_open()
  end subroutine 

  subroutine read_open_i()
    implicit none
    cp_file_number=cp_file_number-1
!    print *, 'reading ', cp_file_number
    call cp_open()
  end subroutine 

  subroutine read_openX_i(X)
    implicit none
    integer X
    cp_file_number=X
!    print *, 'reading ', cp_file_number
    call cp_open()
  end subroutine 

  subroutine open_i()
    implicit none


#ifdef ALLOW_USE_MPI
include "mpif.h" 
#endif

    integer rank, mpirc
    character*128 fname ! file name
    ! get unit
    rank=0
    call cp_findunit()
!    print *, 'OAD: opening CP file ', cp_file_number
    ! construct the file name

#ifdef ALLOW_USE_MPI
    call mpi_comm_rank(MPI_COMM_WORLD,rank, mpirc)
#endif


    write(fname,'(A,I3.3,A,I5.5)') 'oad_reg_cp.',rank,'.',cp_file_number
    open( UNIT=cp_io_unit,FILE=TRIM(fname),FORM='unformatted',STATUS='UNKNOWN' )
  end subroutine 

  subroutine close_i()
    implicit none
    close( UNIT=cp_io_unit)
  end subroutine

  subroutine findunit_i()
    ! returns a valid, unused unit number for Fortran I/O
    ! the routine stops the program if an error occurs in the process
    ! of searching the I/O channels.
    implicit none
    ! Local
    integer ii
    logical op
    integer ios
    character*(1024) msgbuf
    ! Sweep through a valid range of unit numbers
    cp_io_unit=-1
    do ii=9,999
       if (cp_io_unit.eq.-1) then
          inquire(unit=ii,iostat=ios,opened=op)
          if (ios.ne.0) then
             write(msgbuf,'(a,i2.2)')  'OAD_regular_cp:findunit_i: inquiring unit number = ',ii
             print *, msgBuf
             write(msgbuf,'(a)') 'OAD_regular_cp:findunit_i: inquire statement failed!'
             print *, msgBuf
             stop 'ABNORMAL END: S/R OAD_regular_cp:findunit_i'
          endif
          if (.NOT. op) then
             cp_io_unit=ii
          end if
       end if
    end do
    ! Was there an available unit number
    if (cp_io_unit.eq.-1) then
       write(msgbuf,'(a)')  'OAD_regular_cp:findunit_i: could not find an available unit number!'
       print *, msgBuf
       stop 'ABNORMAL END: S/R OAD_regular_cp:findunit_i'
    endif
  end subroutine

  function cp_fNumber()
    integer cp_fNumber
    cp_fNumber=cp_file_number
  end function 

  subroutine open_state_i()
    implicit none
#ifdef ALLOW_USE_MPI
include "mpif.h" 
#endif
    integer rank, mpirc
    character*128 fname ! file name
    rank=0
#ifdef ALLOW_USE_MPI
    call mpi_comm_rank(MPI_COMM_WORLD,rank, mpirc)
#endif
    write(fname,'(A,I3.3)') 'oad_reg_state.',rank
    open( UNIT=cp_state_unit,FILE=TRIM(fname),FORM='formatted',STATUS='UNKNOWN' )
  end subroutine 

  subroutine close_state_i()
    implicit none
    close( UNIT=cp_state_unit)
  end subroutine

  subroutine write_state_i(currcp, curradjointcp, maxfwditer, maxadjiter, myIter)
    implicit none
    integer :: currcp, curradjointcp, maxfwditer, maxadjiter, myIter
    call cp_open_state
    print *, 'DIVA Writing to file state =', currcp, curradjointcp, maxfwditer, maxadjiter, myIter
    write (cp_state_unit, *) currcp
    write (cp_state_unit, *) curradjointcp
    write (cp_state_unit, *) maxfwditer
    write (cp_state_unit, *) maxadjiter
    write (cp_state_unit, *) myIter
    call cp_close_state
  end subroutine

  subroutine read_state_i(currcp, curradjointcp, maxfwditer, maxadjiter, myIter)
    implicit none
    integer :: currcp, curradjointcp, maxfwditer, maxadjiter, myIter
    call cp_open_state
    read (cp_state_unit, *) currcp
    read (cp_state_unit, *) curradjointcp
    read (cp_state_unit, *) maxfwditer
    read (cp_state_unit, *) maxadjiter
    read (cp_state_unit, *) myIter
    print *, 'DIVA Read from file state =', currcp, curradjointcp, maxfwditer, maxadjiter, myIter
    call cp_close_state
  end subroutine 
end module 
