C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_generic.F,v 1.23 2014/10/04 20:28:20 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_generic(
     &     nnzbar, localbarfile, localbar, xx_localbar_mean_dummy,
     &     nnzobs, localobsfile, mult_local,
     &     nrecloc, nrecobs,
     &     localstartdate, localperiod,
     &     ylocmask, localweight,
     &     spminloc, spmaxloc, spzeroloc,
     &     preproc, posproc, scalefile,
     &     outlev, outname,
     &     objf_local, num_local,
     &     myiter, mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_generic
c     ==================================================================
c
c     o Generic routine for evaluating time-dependent
c       cost function contribution
c
c     ==================================================================
c     SUBROUTINE cost_generic
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer nnzbar, nnzobs
      integer nrecloc, nrecobs
      integer myiter
      integer mythid
      integer localstartdate(4)
      integer outlev

      _RL localbar   (1-olx:snx+olx,1-oly:sny+oly,nnzbar,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL xx_localbar_mean_dummy
      _RL mult_local
      _RL mytime
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL objf_local(nsx,nsy)
      _RL num_local(nsx,nsy)

      character*(1) ylocmask
      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile
      character*(16) preproc
      character*(16) posproc
      character*(MAX_LEN_FNAM) scalefile
      character*(MAX_LEN_FNAM) outname

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer  il
      integer localrec, obsrec

      logical doglobalread
      logical ladinit

      _RL localwww
      _RL localcost
      _RL junk

      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL localdif   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL difmask  (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)

      character*(128) fname1, fname2, fname3

      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Initialise local variables.

      localwww = 0. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_local(bi,bj) = 0. _d 0
          num_local(bi,bj) = 0. _d 0
        enddo
      enddo
      call ecco_zero(localobs,nnzobs,zeroRL,myThid)
      call ecco_zero(localdif,nnzobs,zeroRL,myThid)
      call ecco_zero(difmask,nnzobs,zeroRL,myThid)

c--   Assign mask
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx
      if ( ylocmask .EQ. 'C' .OR. ylocmask .EQ. 'c' ) then
         localmask(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
      elseif ( ylocmask .EQ. 'S' .OR. ylocmask .EQ. 's' ) then
         localmask(i,j,k,bi,bj) = maskS(i,j,k,bi,bj)
      elseif ( ylocmask .EQ. 'W' .OR. ylocmask .EQ. 'w' ) then
         localmask(i,j,k,bi,bj) = maskW(i,j,k,bi,bj)
      else
         STOP 'cost_generic: wrong ylocmask'
      endif
              enddo
            enddo
          enddo
        enddo
      enddo

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c-- reset nrecloc, if needed, according to preproc
      if ( preproc .EQ. 'climmon ') nrecloc=MIN(nrecloc,12)

      if ( .NOT. ( localobsfile.EQ.' ' ) ) then

c--   loop over obsfile records
      do irec = 1, nrecloc

c--     determine records and file names
        exst=.FALSE.
        call cost_gencal(localbarfile, localobsfile,
     &     irec, localstartdate, localperiod, fname1,
     &     fname2, localrec, obsrec, exst, mythid )

c--     load model average and observed average
        call cost_genread( fname1, localbar, irec, doglobalread,
     &                     ladinit, eccoiter, nnzbar, preproc,
     &                     mythid, xx_localbar_mean_dummy )

        call ecco_zero(localobs,nnzobs,spzeroloc,myThid)
        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) )
     &  call mdsreadfield( fname2, cost_iprec, cost_yftype, nnzobs,
     &         localobs, localrec, mythid )

c--     Compute masked model-data difference
        call cost_gendif( localbar, nnzbar, localobs, nnzobs,
     &     localmask, spminloc, spmaxloc, spzeroloc, 
     &     posproc, scalefile, localdif, difmask, myThid )

c--     Compute normalized model-obs cost function
        do bj = jtlo,jthi
          do bi = itlo,ithi
            localcost    = 0. _d 0
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
                localwww  = localweight(i,j,k,bi,bj)
     &                    * difmask(i,j,k,bi,bj)
                junk      = localdif(i,j,k,bi,bj)
                localcost = localcost + junk*junk*localwww
                if ( localwww .ne. 0. )
     &               num_local(bi,bj) = num_local(bi,bj) + 1. _d 0
              enddo
             enddo
            enddo
            objf_local(bi,bj) = objf_local(bi,bj) + localcost
          enddo
        enddo



c--     output model-data difference to disk
        if ( outlev.GT.0 ) then
          il=ilnblnk(outname)
          write(fname3(1:128),'(2a)') 'misfit_', outname(1:il)
          if ( nnzobs.EQ.1 ) CALL
     &    WRITE_REC_XY_RL( fname3, localdif,irec, eccoiter, mythid )
          if ( nnzobs.EQ.nr ) CALL
     &    WRITE_REC_XYZ_RL( fname3, localdif,irec, eccoiter, mythid )
        endif

      enddo
c--   End of loop over obsfile records.

      endif

#endif /* ALLOW_ECCO */

      end
