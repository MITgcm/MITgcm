C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_ssh_mean.F,v 1.5 2005/05/27 22:10:26 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ssh_mean(
     I                     psmean, offset
     O                   , costmean
     I                   , mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_ssh_mean
c     ==================================================================
c
c     o Evaluate cost function contribution of sea surface height.
c       using of geoid error covariances requires regular model grid
c     o TODO: interpolate model grid to regular grid
c             check mask
c
c     started: Detlef Stammer, Ralf Giering Jul-1996
c     changed: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c              - totally rewrite for parallel processing
c              heimbach@mit.edu 13-Mar-2002
c              - several wrong i-loop boundaries (spotted by G. Gebbie)
c              - nprocs need to be replaced by nPx*nPy
c              - geoid error does not work as of now
c              heimbach@mit.edu 05-May-2005
c              - debugged and restructuted
c
c     ==================================================================
c     SUBROUTINE cost_ssh_mean
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "ecco_cost.h"
#include "optim.h"

#ifdef ALLOW_EGM96_ERROR_COV
# if (defined (ALLOW_USE_MPI) || defined (ALWAYS_USE_MPI))
#  include "EESUPPORT.h"
# else
      INTEGER nProcs
      PARAMETER (nProcs=1)
# endif
# include "sphere.h"
#endif

c     == routine arguments ==

      _RL     psmean ( 1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy )
      _RL     offset
      _RL     costmean
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

cph(
      _RL diagnosfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
cph)

#ifdef ALLOW_EGM96_ERROR_COV
      _RL cphi
      _RL xmean
      _RL shc( ncshc )
      _RL misfit  ( 1-olx:snx+olx, 1-oly:sny+oly, nsx,nsy )
      _RL misfitgl( 1-olx:snx+olx, 1-oly:sny+oly, nsx,nsy, npx,npy )

      _RL pwork     ( (lshcmax+1)*(lshcmax+2)/2 )
      _RL diffearth ( lonshc, latshc )
      integer joffset
      integer ipx, ipy
      integer iglobe, jglobe
      integer iproc
      integer mpirc
      integer mpicrd(2)
#endif

      _RL diff
      Real*8 sumc
      character*(max_len_mbuf) msgbuf

c     == end of interface ==

c--   Initialise local variables.
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      costmean   = 0.

#ifdef ALLOW_EGM96_ERROR_COV

c--   compute misfit on local domain
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
               if ( tpmeanmask(i,j,bi,bj) .gt. 0. ) then
                 misfit(i,j,bi,bj) =
     &           tpmean(i,j,bi,bj) - offset - psmean(i,j,bi,bj)
              else
                 misfit(i,j,bi,bj) = 0.
              endif
            enddo
          enddo
        enddo
      enddo

c--   communicate to get misfit on full 2d model surface grid
#if (defined (ALLOW_USE_MPI) || defined (ALWAYS_USE_MPI))
      call exch_all_2d_rl( misfit, misfitgl, mythid )
#else
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
               misfitgl(i,j,bi,bj,1,1) = misfit(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

c--   set meridional index offset,
c--   it is non zero if the grid does no reach the poles
      joffset = (latshc - ny)/2

      write(msgbuf,'(a,I10)')
     &     'cost_ssh_mean: grid starts at point ', joffset
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c--   preset field
c--   necessary if the grid does no reach the poles
      do j = 1, latshc
        do i = 1, lonshc
          diffearth(i,j) = 0.
        enddo
      enddo

c--   -interpolate- from model grid to regular grid
c--   so far we assume that the model grid is already regular
      _BEGIN_MASTER( mythid )

      do iproc = 1, nprocs

#if (defined (ALLOW_USE_MPI) || defined (ALWAYS_USE_MPI))
c--     get coordinates of processor (iporc-1)
        call MPI_Cart_coords(
     I          MPI_COMM_MODEL, iproc-1, 2, mpicrd
     O        , mpirc
     &        )
        ipx = 1 + mpicrd(1)
        ipy = 1 + mpicrd(2)
#else
        ipx = 1
        ipy = 1
#endif

        do bj = jtlo,jthi
          do bi = itlo,ithi

            do j = jmin,jmax
              do i = imin,imax
                jglobe = joffset+ j + sNy*(bj-1) + sNy*nSy*(ipy-1)
                iglobe =          i + sNx*(bi-1) + sNx*nSx*(ipx-1)

                diffearth(iglobe,jglobe) = misfitgl(i,j,bi,bj,ipx,ipy)
              enddo
            enddo

          enddo
        enddo

      enddo

c--   Project regular grid misfit onto sperical harmonics
      call shc4grid(
     I               lshcmax
     O             , shc
     I             , latshc, lonshc
     I             , diffearth
     W             , pwork
     &             )

c--   Remove the C(0,0) component, i.e. the global mean.
      shc(1) = 0.

C--   Compute the cost function for the mean SSH.
      call cost_geoid(
     O                 costmean
     I               , shc
     I               , mythid
     &               )

      _END_MASTER( mythid )

      _BARRIER

#else

c--   Compute cost function for SSH by using the diagonal
c--   of the error covariance only.
c--
c--   Note: wp is assumed to include latitude dependence
c--         of error due to meridian convergence;
c--         --> no weighting by cosphi.

      sumc = 0.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              diff = psmean(i,j,bi,bj) - tpmean(i,j,bi,bj) + offset
              sumc = sumc + diff*diff
     &                    * wp(i,j,bi,bj)*tpmeanmask(i,j,bi,bj)
              if ( wp(i,j,bi,bj)*tpmeanmask(i,j,bi,bj) .ne. 0. )
     &             num_hmean = num_hmean + 1. _d 0
	      diagnosfld(i,j,bi,bj) = diff*diff
     &                    * wp(i,j,bi,bj)*tpmeanmask(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

cph(
      CALL WRITE_FLD_XY_RL( 'DiagnosSSHmean', ' ', diagnosfld,
     &                           optimcycle, mythid )
cph)

c--   Do the global summation.
      _GLOBAL_SUM_R8( sumc, mythid )
      _GLOBAL_SUM_R8( num_hmean, mythid )
      costmean = sumc

#endif

      end
