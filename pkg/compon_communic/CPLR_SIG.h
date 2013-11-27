C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/CPLR_SIG.h,v 1.3 2013/11/27 21:48:30 jmc Exp $
C $Name:  $

! Special meanings/handles
      integer   MITCPLR_COUPLER
      parameter(MITCPLR_COUPLER=59)

      logical DEBUG
      parameter (DEBUG=.TRUE.)
      logical VERB
c     parameter (VERB=.TRUE.)
      parameter (VERB=.FALSE.)

! Parameters for fixed length declarations
      integer MAX_PROCS
      parameter(MAX_PROCS=128)

      integer MAX_IBUF
      parameter(MAX_IBUF=1024)

      integer MAXLEN_COMP_NAME
      parameter( MAXLEN_COMP_NAME=32 )

      integer MAX_COMPONENTS
      parameter( MAX_COMPONENTS=10 )

      integer MAX_TILES
      parameter( MAX_TILES=16 )

      integer HEADER_SIZE
      parameter( HEADER_SIZE=8+MAXLEN_COMP_NAME )

      integer MAX_R4_BUFLEN
      parameter( MAX_R4_BUFLEN=HEADER_SIZE+256*256 )

      integer MAX_R8_BUFLEN
      parameter( MAX_R8_BUFLEN=HEADER_SIZE+256*256 )

      integer LogUnit
      parameter( LogUnit=99 )

! Temporary arrays for local use: now declared locally where needed
c     integer ibuf(MAX_IBUF)

! The list of components
      integer num_components
      character*(MAXLEN_COMP_NAME) component_List(MAX_COMPONENTS)
      common /CPLR_COMP_LIST/
     &  num_components,
     &  component_List

! Coupler has duplicate copy of details
      integer num_coupler_procs
      integer rank_coupler_procs(MAX_PROCS)
      character*(MAXLEN_COMP_NAME) coupler_Name
      common /CPLR_COUPLER/
     &  num_coupler_procs,
     &  rank_coupler_procs,
     &  coupler_Name

! Buffers
      real*4 r4buf(MAX_R4_BUFLEN)
      real*8 r8buf(MAX_R8_BUFLEN)
      common /CPLR_BUFFERS/
     &  r4buf,r8buf

! Information for communicating with my two communicators
      integer my_component_ind
      integer my_rank_in_world
      integer MPI_COMM_mylocal
      integer num_procs_in_local
      integer my_rank_in_local
      integer MPI_COMM_myglobal
      integer num_procs_in_global
      integer my_rank_in_global
      integer my_coupler_rank
      integer my_num_tiles
      integer my_tile_nx(MAX_TILES)
      integer my_tile_ny(MAX_TILES)
      integer my_tile_i0(MAX_TILES)
      integer my_tile_j0(MAX_TILES)
      integer my_tile_bi(MAX_TILES)
      integer my_tile_bj(MAX_TILES)
      character*(MAXLEN_COMP_NAME) my_component_name
      common /CPLR_IDENT/
     &  my_component_ind,
     &  my_rank_in_world,
     &  MPI_COMM_mylocal,
     &  num_procs_in_local,
     &  my_rank_in_local,
     &  MPI_COMM_myglobal,
     &  num_procs_in_global,
     a  my_rank_in_global,
     &  my_coupler_rank,
     &  my_num_tiles,
     &  my_tile_nx,
     &  my_tile_ny,
     &  my_tile_i0,
     &  my_tile_j0,
     &  my_tile_bi,
     &  my_tile_bj,
     &  my_component_name

! The components details
      integer num_component_procs(MAX_COMPONENTS)
      integer rank_component_procs(MAX_PROCS,MAX_COMPONENTS)
      integer num_compcplr_procs(MAX_COMPONENTS)
      integer rank_compcplr_procs(MAX_PROCS,MAX_COMPONENTS)
      integer MPI_COMM_component(MAX_COMPONENTS)
      integer MPI_COMM_compcplr(MAX_COMPONENTS)
      integer component_num_tiles(MAX_PROCS,MAX_COMPONENTS)
      integer component_tile_nx(MAX_TILES,MAX_PROCS,MAX_COMPONENTS)
      integer component_tile_ny(MAX_TILES,MAX_PROCS,MAX_COMPONENTS)
      integer component_tile_i0(MAX_TILES,MAX_PROCS,MAX_COMPONENTS)
      integer component_tile_j0(MAX_TILES,MAX_PROCS,MAX_COMPONENTS)
      character*(MAXLEN_COMP_NAME) component_Name(MAX_COMPONENTS)
      common /CPLR_COMPONENTS/
     &  num_component_procs,
     &  rank_component_procs,
     &  num_compcplr_procs,
     &  rank_compcplr_procs,
     &  MPI_COMM_component,
     &  MPI_COMM_compcplr,
     &  component_num_tiles,
     &  component_tile_nx,
     &  component_tile_ny,
     &  component_tile_i0,
     &  component_tile_j0,
     &  component_Name
