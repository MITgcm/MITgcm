C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_generic.F,v 1.25 2014/10/07 12:22:55 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_generic
C     !INTERFACE:
      subroutine cost_generic(
     &     nnzbar, localbarfile, localbar, xx_localbar_mean_dummy,
     &     nnzobs, localobsfile, mult_local,
     &     nrecloc, nrecobs,
     &     localstartdate, localperiod,
     &     ylocmask, localweight,
     &     spminloc, spmaxloc, spzeroloc,
     &     preproc, preproc_c, preproc_i, preproc_r,
     &     posproc, posproc_c, posproc_i, posproc_r,
     &     outlev, outname,
     &     objf_local, num_local,
     &     myiter, mytime, mythid )

C     !DESCRIPTION: \bv
C     Generic routine for evaluating time-dependent
c       cost function contribution
C     \ev

C     !USES:
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

      integer myiter
      integer mythid
      integer nnzbar, nnzobs
      integer nrecloc, nrecobs
      integer localstartdate(4)
      integer outlev
      integer preproc_i(NGENPPROC)
      integer posproc_i(NGENPPROC)

      _RL objf_local(nsx,nsy)
      _RL num_local(nsx,nsy)
      _RL localbar   (1-olx:snx+olx,1-oly:sny+oly,nnzbar,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL xx_localbar_mean_dummy
      _RL mult_local
      _RL mytime
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL preproc_r(NGENPPROC)
      _RL posproc_r(NGENPPROC)

      character*(1) ylocmask
      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc_c(NGENPPROC)
      character*(MAX_LEN_FNAM) outname

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer irec
      integer  il
      integer localrec, obsrec

      logical doglobalread
      logical ladinit

      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL localdif   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL difmask    (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)

      character*(128) fname1, fname2, fname3

      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c--   Initialise local variables.

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
      if ( ylocmask .EQ. 'C' .OR. ylocmask .EQ. 'c' ) then
        call ecco_cp(maskC,nr,localmask,nr,myThid)
      elseif ( ylocmask .EQ. 'S' .OR. ylocmask .EQ. 's' ) then
        call ecco_cp(maskS,nr,localmask,nr,myThid)
      elseif ( ylocmask .EQ. 'W' .OR. ylocmask .EQ. 'w' ) then
        call ecco_cp(maskW,nr,localmask,nr,myThid)
      else
         STOP 'cost_generic: wrong ylocmask'
      endif

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

c-- reset nrecloc, if needed, according to preproc
      if ( preproc(1) .EQ. 'climmon') nrecloc=MIN(nrecloc,12)

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
        call ecco_diff( localbar, nnzbar, localobs, nnzobs,
     &     localmask, spminloc, spmaxloc, spzeroloc, 
     &     localdif, difmask, myThid )

c--     Compute normalized model-obs cost function
        call ecco_addcost(
     I                   localdif, localweight, difmask, nnzobs,
     I                   objf_local, num_local,
     I                   myThid
     &                   )

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
