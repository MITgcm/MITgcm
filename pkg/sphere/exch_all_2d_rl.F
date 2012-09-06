C $Header: /u/gcmpack/MITgcm/pkg/sphere/exch_all_2d_rl.F,v 1.3 2012/09/06 14:55:39 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      subroutine exch_allgather_2d_rl(
     I       arr
     O     , full
     I     , myThid
     &     )

c     ==================================================================
c     SUBROUTINE exch_allgather_2d_rl
c     ==================================================================
c
c     o exchange local domains of a distributed 2d field
c       so that every processor has the whole field
c
c     started: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c
c     ==================================================================
c     SUBROUTINE exch_allgather_2d_rl
c     ==================================================================
      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "EESUPPORT.h"
#include "EXCH.h"

c     == routine arguments ==

      _RL arr ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL full( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy, nPx, nPy )
      integer myThid

c     == local variables ==
#ifdef ALLOW_USE_MPI
      integer mpirc
      integer mpicrd(2)
      integer ipx, ipy

      _RL recvbuf( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy, nPx*nPy )

      integer    sendsize
      parameter( sendsize = sNx*sNy*nSx*nSy )
      integer    recvsize
      parameter( recvsize = sNx*sNy*nSx*nSy )

      integer iproc
      integer bi, bj
      integer i, j
#endif

C--   Can not start until everyone is ready
      _BARRIER

c--   Only the master thread is doing communication
      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
      IF ( usingMPI ) THEN

      call MPI_Allgather(  arr    , sendsize, MPI_DOUBLE_PRECISION
     &                   , recvbuf, recvsize, MPI_DOUBLE_PRECISION
     &                   , MPI_COMM_MODEL, mpiRC
     &                   )

c--   arrange array according to cartesian coordinates of processors
      do iproc = 1, numberOfProcs

c--     get coordinates of processor (iporc-1)
        call MPI_Cart_coords(
     I          MPI_COMM_MODEL, iproc-1, 2, mpicrd
     O        , mpirc
     &        )

        ipx = 1 + mpicrd(1)
        ipy = 1 + mpicrd(2)

        do bj = 1, nSy
          do bi = 1, nSx
            do j = 1, sNy
              do i = 1, sNx
                full(i,j,bi,bj,ipx,ipy) = recvbuf(i,j,bi,bj,iproc)
              enddo
            enddo
          enddo
        enddo

      enddo

      ENDIF
#endif /* ALLOW_USE_MPI */

c--   end of master thread only computations
      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END
