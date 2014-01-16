module OAD_tape

  implicit none

  private :: increment , dtt, itt, ltt, stt, & 
       init, dump_tapestats, & 
       dt_grow, it_grow, lt_grow, st_grow, &
       push_d0, push_i0, push_d1, push_i1, & 
       pop_d0, pop_i0, pop_d1, pop_i1, & 
       push_d4, push_d6, & 
       pop_d4, pop_d6
    
  public :: &
       oad_dt, oad_dt_ptr, oad_dt_sz, oad_dt_grow, &
       oad_it, oad_it_ptr, oad_it_sz, oad_it_grow, &
       oad_lt, oad_lt_ptr, oad_lt_sz, oad_lt_grow, &
       oad_st, oad_st_ptr, oad_st_sz, oad_st_grow, &
       oad_chunk_size, &
       oad_tape_init, &
       oad_dump_tapestats, & 
       oad_tape_push, oad_tape_pop
       
  double precision, dimension(:), allocatable :: oad_dt, dtt
  integer, dimension(:), allocatable :: oad_it, itt
  logical, dimension(:), allocatable :: oad_lt, ltt
  character(80), dimension(:), allocatable :: oad_st, stt
  integer :: oad_dt_ptr=0, oad_it_ptr=0
  integer :: oad_dt_sz=0, oad_it_sz=0
  integer :: oad_lt_ptr=0, oad_st_ptr=0
  integer :: oad_lt_sz=0, oad_st_sz=0
  integer :: increment
  integer :: oad_chunk_size

  interface oad_tape_init
    module procedure init
  end interface 

  interface oad_dump_tapestats
     module procedure dump_tapestats
  end interface

  interface oad_dt_grow
    module procedure dt_grow
  end interface

  interface oad_it_grow
    module procedure it_grow
  end interface

  interface oad_lt_grow
    module procedure lt_grow
  end interface

  interface oad_st_grow
    module procedure st_grow
  end interface

  interface oad_tape_push
     module procedure push_d0, push_i0
     module procedure push_d1, push_i1
     module procedure push_d4, push_d6
  end interface

  interface oad_tape_pop
     module procedure pop_d0, pop_i0
     module procedure pop_d1, pop_i1
     module procedure pop_d4, pop_d6
  end interface

contains

  subroutine init
    integer :: initialSize=1048576
    increment=16777216
    ! DT
    oad_dt_ptr=1
    if (allocated(oad_dt)) then 
       deallocate(oad_dt)
    end if
    oad_dt_sz=initialSize
    allocate(oad_dt(oad_dt_sz))
    ! IT
    oad_it_ptr=1
    if (allocated(oad_it)) then 
       deallocate(oad_it)
    end if
    oad_it_sz=initialSize
    allocate(oad_it(oad_it_sz))
    ! LT
    oad_lt_ptr=1
    if (allocated(oad_lt)) then 
       deallocate(oad_lt)
    end if
    oad_lt_sz=initialSize
    allocate(oad_lt(oad_lt_sz))
    ! ST
    oad_st_ptr=1
    if (allocated(oad_st)) then 
       deallocate(oad_st)
    end if
    oad_st_sz=initialSize
    allocate(oad_st(oad_st_sz))
  end subroutine init

  subroutine dump_tapestats()
    write(*,'(3(A,I9))',ADVANCE='NO') & 
         ' TD:',oad_dt_ptr,' TI:',oad_it_ptr, ' TS:',oad_st_ptr
  end subroutine dump_tapestats

  subroutine dt_grow
    integer status
    print *, "OAD: DT+ ", oad_dt_sz
    allocate(dtt(oad_dt_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed with', status
       stop
    end if
    dtt=oad_dt
    deallocate(oad_dt)
    allocate(oad_dt(oad_dt_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed with', status
       stop
    end if
    oad_dt(1:oad_dt_sz) = dtt
    deallocate(dtt)
    oad_dt_sz=oad_dt_sz+increment
  end subroutine dt_grow

  subroutine it_grow
    integer status
    print *, "OAD: IT+ ", oad_it_sz
    allocate(itt(oad_it_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed with', status
       stop
    end if
    itt=oad_it
    deallocate(oad_it)
    allocate(oad_it(oad_it_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed with', status
       stop
    end if
    oad_it(1:oad_it_sz) = itt
    deallocate(itt)
    oad_it_sz=oad_it_sz+increment
  end subroutine it_grow

  subroutine lt_grow
    integer status
    print *, "OAD: LT+ ", oad_lt_sz
    allocate(ltt(oad_lt_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed wlth', status
       stop
    end if
    ltt=oad_lt
    deallocate(oad_lt)
    allocate(oad_lt(oad_lt_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed wlth', status
       stop
    end if
    oad_lt(1:oad_lt_sz) = ltt
    deallocate(ltt)
    oad_lt_sz=oad_lt_sz+increment
  end subroutine lt_grow

  subroutine st_grow
    integer status
    print *, "OAD: ST+ ", oad_st_sz
    allocate(stt(oad_st_sz),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (1)failed wsth', status
       stop
    end if
    stt=oad_st
    deallocate(oad_st)
    allocate(oad_st(oad_st_sz+increment),STAT=status)
    if (status .gt. 0 ) then
       print *,'OAD: allocation (2)failed wsth', status
       stop
    end if
    oad_st(1:oad_st_sz) = stt
    deallocate(stt)
    oad_st_sz=oad_st_sz+increment
  end subroutine st_grow

  subroutine push_d0(v)
    implicit none
    double precision :: v
    if(oad_dt_sz .lt. oad_dt_ptr+1) call oad_dt_grow()
    oad_dt(oad_dt_ptr)=v; oad_dt_ptr=oad_dt_ptr+1
  end subroutine push_d0

  subroutine push_i0(v)
    implicit none
    integer :: v
    if(oad_it_sz .lt. oad_it_ptr+1) call oad_it_grow()
    oad_it(oad_it_ptr)=v; oad_it_ptr=oad_it_ptr+1
  end subroutine push_i0

  subroutine push_d1(v)
    implicit none
    double precision :: v(:)
    integer :: chunk
    chunk=size(v,1)
    if(oad_dt_sz .lt. oad_dt_ptr+chunk) call oad_dt_grow()
    oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1)=v; oad_dt_ptr=oad_dt_ptr+chunk
  end subroutine push_d1

  subroutine push_i1(v)
    implicit none
    integer :: v(:)
    integer :: chunk
    chunk=size(v,1)
    if(oad_it_sz .lt. oad_it_ptr+chunk) call oad_it_grow()
    oad_it(oad_it_ptr:oad_it_ptr+chunk-1)=v; oad_it_ptr=oad_it_ptr+chunk
  end subroutine push_i1

  subroutine push_d4(v)
    implicit none
    double precision :: v(:,:,:,:)
    integer :: chunk(1), dims(4)
    dims=shape(v)
    chunk(1)=dims(1)*dims(2)*dims(3)*dims(4)
    do while (oad_dt_sz .lt. oad_dt_ptr+chunk(1)) 
       call oad_dt_grow()
    end do
    oad_dt(oad_dt_ptr:oad_dt_ptr+chunk(1)-1)=reshape(v,chunk) 
    oad_dt_ptr=oad_dt_ptr+chunk(1)
  end subroutine push_d4

  subroutine push_d6(v)
    implicit none
    double precision :: v(:,:,:,:,:,:)
    integer :: chunk(1), dims(6)
    dims=shape(v)
    chunk(1)=dims(1)*dims(2)*dims(3)*dims(4)*dims(5)*dims(6)
    do while (oad_dt_sz .lt. oad_dt_ptr+chunk(1)) 
       call oad_dt_grow()
    end do
    oad_dt(oad_dt_ptr:oad_dt_ptr+chunk(1)-1)=reshape(v,chunk) 
    oad_dt_ptr=oad_dt_ptr+chunk(1)
  end subroutine push_d6

  subroutine pop_d0(v)
    implicit none
    double precision :: v
    oad_dt_ptr=oad_dt_ptr-1
    v=oad_dt(oad_dt_ptr)
  end subroutine pop_d0

  subroutine pop_i0(v)
    implicit none
    integer :: v
    oad_it_ptr=oad_it_ptr-1
    v=oad_it(oad_it_ptr)
  end subroutine pop_i0

  subroutine pop_d1(v)
    implicit none
    double precision :: v(:)
    integer :: chunk
    chunk=size(v,1)
    oad_dt_ptr=oad_dt_ptr-chunk    
    v=oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1)
  end subroutine pop_d1
  
  subroutine pop_i1(v)
    implicit none
    integer :: v(:)
    integer :: chunk
    chunk=size(v,1)
    oad_it_ptr=oad_it_ptr-chunk
    v=oad_it(oad_it_ptr:oad_it_ptr+chunk-1)
  end subroutine pop_i1

  subroutine pop_d4(v)
    implicit none
    double precision :: v(:,:,:,:)
    integer :: chunk, dims(4)
    dims=shape(v)
    chunk=dims(1)*dims(2)*dims(3)*dims(4)
    oad_dt_ptr=oad_dt_ptr-chunk
    v=reshape(oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1),dims) 
  end subroutine pop_d4

  subroutine pop_d6(v)
    implicit none
    double precision :: v(:,:,:,:,:,:)
    integer :: chunk, dims(6)
    dims=shape(v)
    chunk=dims(1)*dims(2)*dims(3)*dims(4)*dims(5)*dims(6)
    oad_dt_ptr=oad_dt_ptr-chunk
    v=reshape(oad_dt(oad_dt_ptr:oad_dt_ptr+chunk-1),dims) 
  end subroutine pop_d6

end module
