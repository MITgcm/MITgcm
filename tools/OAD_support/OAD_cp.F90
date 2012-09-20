module OAD_cp

  implicit none

  private :: cp_file_number, cp_open

  public :: cp_io_unit,  cp_init, cp_write_open, cp_read_open, cp_close

  integer :: cp_file_number, cp_io_unit

  interface cp_init
     module procedure init_i
  end interface

  interface cp_open
     module procedure open_i
  end interface

  interface cp_write_open
     module procedure write_open_i
  end interface

  interface cp_read_open
     module procedure read_open_i
  end interface

  interface cp_close
     module procedure close_i
  end interface

  interface cp_findunit
     module procedure findunit_i
  end interface

contains

  subroutine init_i
    implicit none
    cp_file_number=1
  end subroutine

  subroutine write_open_i()
    implicit none
    call cp_open()
    cp_file_number=cp_file_number+1
  end subroutine 

  subroutine read_open_i()
    implicit none
    cp_file_number=cp_file_number-1
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
    write(fname,'(A,I3.3,A,I5.5)') 'oad_cp.',rank,'.',cp_file_number
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
             write(msgbuf,'(a,i2.2)')  'OAD_cp:findunit_i: inquiring unit number = ',ii
             print *, msgBuf
             write(msgbuf,'(a)') 'OAD_cp:findunit_i: inquire statement failed!'
             print *, msgBuf
             stop 'ABNORMAL END: S/R OAD_cp:findunit_i'
          endif
          if (.NOT. op) then
             cp_io_unit=ii
          end if
       end if
    end do
    ! Was there an available unit number
    if (cp_io_unit.eq.-1) then
       write(msgbuf,'(a)')  'OAD_cp:findunit_i: could not find an available unit number!'
       print *, msgBuf
       stop 'ABNORMAL END: S/R OAD_cp:findunit_i'
    endif
  end subroutine

end module 
