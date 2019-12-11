#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_generic
C     !INTERFACE:
      subroutine cost_generic(
     &     nnzbar, localbarfile, dummy,
     &     nnzobs, localobsfile, localerrfile,
     &     mult_local, nrecloc, nrecobs,
     &     localstartdate, localperiod,
     &     ylocmask, spminloc, spmaxloc, spzeroloc,
     &     preproc, preproc_c, preproc_i, preproc_r,
     &     posproc, posproc_c, posproc_i, posproc_r,
     &     outlev, outname,
     &     objf_local, num_local,
     &     myIter, myTime, myThid )

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

      integer myIter
      integer myThid
      integer nnzbar, nnzobs
      integer nrecloc, nrecobs
      integer localstartdate(4)
      integer outlev
      integer preproc_i(NGENPPROC)
      integer posproc_i(NGENPPROC)

      _RL objf_local(nSx,nSy)
      _RL num_local(nSx,nSy)
      _RL dummy
      _RL mult_local
      _RL myTime
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL preproc_r(NGENPPROC)
      _RL posproc_r(NGENPPROC)

      character*(1) ylocmask
      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile
      character*(MAX_LEN_FNAM) localerrfile
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc_c(NGENPPROC)
      character*(MAX_LEN_FNAM) outname

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj,k2
      integer itlo,ithi
      integer jtlo,jthi
      logical domean, doanom

      _RL localdifmean1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localdifmean2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

c--   Initialise local variables.

      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_local(bi,bj) = 0. _d 0
          num_local(bi,bj) = 0. _d 0
        enddo
      enddo

      call ecco_zero(localdifmean1,Nr,zeroRL,myThid)
      call ecco_zero(localdifmean2,Nr,zeroRL,myThid)

      domean=.FALSE.
      doanom=.FALSE.
      do k2 = 1, NGENPPROC
          if (preproc(k2).EQ.'mean') domean=.TRUE.
          if (preproc(k2).EQ.'anom') doanom=.TRUE.
      enddo

C Extra time loop to compute time-mean fields and costs
      if ( (.NOT. ( localobsfile.EQ.' ' ) )
     &   .AND. ( domean .OR. doanom ) ) then
        call cost_genloop(
     &     localdifmean1,localdifmean2,.FALSE.,
     &     nnzbar, localbarfile, dummy,
     &     nnzobs, localobsfile, localerrfile,
     &     mult_local, nrecloc, nrecobs,
     &     localstartdate, localperiod,
     &     ylocmask, spminloc, spmaxloc, spzeroloc,
     &     preproc, preproc_c, preproc_i, preproc_r,
     &     posproc, posproc_c, posproc_i, posproc_r,
     &     outlev, outname,
     &     objf_local, num_local,
     &     myIter, myTime, myThid )
      endif

      call ecco_zero(localdifmean1,Nr,zeroRL,myThid)

      if ((.NOT.(localobsfile.EQ.' ')).AND.(.NOT.domean)) then
        call cost_genloop(
     &     localdifmean2,localdifmean1,.TRUE.,
     &     nnzbar, localbarfile, dummy,
     &     nnzobs, localobsfile, localerrfile,
     &     mult_local, nrecloc, nrecobs,
     &     localstartdate, localperiod,
     &     ylocmask, spminloc, spmaxloc, spzeroloc,
     &     preproc, preproc_c, preproc_i, preproc_r,
     &     posproc, posproc_c, posproc_i, posproc_r,
     &     outlev, outname,
     &     objf_local, num_local,
     &     myIter, myTime, myThid )
      endif

#endif /* ALLOW_ECCO */

      return
      end

C--------------

      subroutine cost_genloop(
     &     localdifmeanIn,localdifmeanOut, addVariaCost,
     &     nnzbar, localbarfile, dummy,
     &     nnzobs, localobsfile, localerrfile,
     &     mult_local, nrecloc, nrecobs,
     &     localstartdate, localperiod,
     &     ylocmask, spminloc, spmaxloc, spzeroloc,
     &     preproc, preproc_c, preproc_i, preproc_r,
     &     posproc, posproc_c, posproc_i, posproc_r,
     &     outlev, outname,
     &     objf_local, num_local,
     &     myIter, myTime, myThid )

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

      integer myIter
      integer myThid
      integer nnzbar, nnzobs
      integer nrecloc, nrecobs
      integer localstartdate(4)
      integer outlev
      integer preproc_i(NGENPPROC)
      integer posproc_i(NGENPPROC)

      _RL objf_local(nSx,nSy)
      _RL num_local(nSx,nSy)
      _RL dummy
      _RL mult_local
      _RL myTime
      _RL localperiod
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL preproc_r(NGENPPROC)
      _RL posproc_r(NGENPPROC)

      character*(1) ylocmask
      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile
      character*(MAX_LEN_FNAM) localerrfile
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc_c(NGENPPROC)
      character*(MAX_LEN_FNAM) outname

      logical addVariaCost
      _RL localdifmeanIn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localdifmeanOut  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer i,j
      integer irec, jrec
      integer il, k2
      integer localrec, obsrec
      integer nrecloop, nrecclim, k2smooth
      logical domean, doanom, dovarwei, doclim, dosmooth, dosumsq

      _RL localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localtmp   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localdif   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL difmask    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      _RL localdifmsk   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localdifsum   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localdifnum   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      _RL mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL fac

      character*(128) fname1, fname2, fname3
      character*200 msgbuf

      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

CEOP

      call ecco_zero(localbar,Nr,zeroRL,myThid)
      call ecco_zero(localweight,Nr,zeroRL,myThid)
      call ecco_zero(localtmp,Nr,zeroRL,myThid)
      call ecco_zero(localmask,Nr,zeroRL,myThid)

      call ecco_zero(localobs,Nr,zeroRL,myThid)
      call ecco_zero(localdif,Nr,zeroRL,myThid)
      call ecco_zero(difmask,Nr,zeroRL,myThid)

      call ecco_zero(localdifmsk,Nr,zeroRL,myThid)
      call ecco_zero(localdifsum,Nr,zeroRL,myThid)
      call ecco_zero(localdifnum,Nr,zeroRL,myThid)

      dosumsq=.TRUE.
      domean=.FALSE.
      doanom=.FALSE.
      dovarwei=.FALSE.
      dosmooth=.FALSE.
      k2smooth=1
      doclim=.FALSE.
      nrecclim=nrecloc
      fac=oneRL

      do k2 = 1, NGENPPROC
          if (preproc(k2).EQ.'mean') domean=.TRUE.
          if (preproc(k2).EQ.'anom') doanom=.TRUE.
          if (preproc(k2).EQ.'variaweight') dovarwei=.TRUE.
          if (preproc(k2).EQ.'nosumsq') dosumsq=.FALSE.
          if (posproc(k2).EQ.'smooth') then
            dosmooth=.TRUE.
            k2smooth=k2
          endif
          if (preproc(k2).EQ.'clim') then
            doclim=.TRUE.
            nrecclim=preproc_i(k2)
          endif
          if (preproc(k2).EQ.'factor') then
            fac=preproc_r(k2)
          endif
      enddo

c--   Assign mask
      if ( ylocmask .EQ. 'C' .OR. ylocmask .EQ. 'c' ) then
        call ecco_cprsrl(maskC,nr,localmask,nr,myThid)
      elseif ( ylocmask .EQ. 'S' .OR. ylocmask .EQ. 's' ) then
        call ecco_cprsrl(maskS,nr,localmask,nr,myThid)
      elseif ( ylocmask .EQ. 'W' .OR. ylocmask .EQ. 'w' ) then
        call ecco_cprsrl(maskW,nr,localmask,nr,myThid)
      else
         STOP 'cost_generic: wrong ylocmask'
      endif



c-- set nrecloop to nrecloc
      nrecloop=nrecloc

c-- reset nrecloop, if needed, according to preproc
      if ( doclim ) nrecloop=MIN(nrecloop,nrecclim)

c--   loop over obsfile records
      do irec = 1, nrecloop

c--     load weights
        exst=.FALSE.
        jrec=1
        if( dovarwei ) jrec = irec
        call cost_gencal(localbarfile, localerrfile,
     &     jrec, localstartdate, localperiod, fname1,
     &     fname3, localrec, obsrec, exst, myThid )
        call ecco_zero(localweight,nnzobs,zeroRL,myThid)
        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) )
     &  call ecco_readwei(fname3,localweight,
     &                    localrec,nnzobs,dosumsq,myThid)

c--     determine records and file names
        exst=.FALSE.
        call cost_gencal(localbarfile, localobsfile,
     &     irec, localstartdate, localperiod, fname1,
     &     fname2, localrec, obsrec, exst, myThid )

c--     load model average and observed average
        call ecco_zero(localbar,nnzbar,zeroRL,myThid)
        call cost_genread( fname1, localbar, localtmp, irec, nnzbar,
     &       nrecloc, preproc, preproc_c, preproc_i, preproc_r,
     &       dummy, myThid )
        call ecco_mult(localbar,nnzbar,fac,myThid)

        call ecco_zero(localobs,nnzobs,spzeroloc,myThid)
        if ( (localrec .GT. 0).AND.(obsrec .GT. 0).AND.(exst) )
     &    CALL READ_REC_3D_RL( fname2, cost_iprec, nnzobs,
     &                         localobs, localrec, 1, myThid )

c--     Compute masked model-data difference
        call ecco_diffmsk( localbar, nnzbar, localobs, nnzobs,
     &     localmask, spminloc, spmaxloc, spzeroloc,
     &     localdif, difmask, myThid )

        if ( doanom ) call ecco_subtract( localdifmeanIn,
     &     nnzobs, localdif, nnzobs, myThid )

        if ( domean.OR.doanom )
     &    call ecco_addmask(localdif,difmask, nnzobs,localdifsum,
     &    localdifnum, nnzobs,myThid)

#ifdef ALLOW_SMOOTH
c--      For 2D smooth routines, use mask2D
c--      should this be ylocmask?
C--      for now, use maskC to reproduce old behavior
         do bj = myByLo(myThid), myByHi(myThid)
          do bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1-OLy,sNy+OLy
            do i = 1-OLx,sNx+OLx
             mask2D(i,j,bi,bj) = maskC(i,j,1,bi,bj)
            enddo
           enddo
          enddo
         enddo
#endif

        if (addVariaCost) then

#ifdef ALLOW_SMOOTH
        if ( useSMOOTH.AND.dosmooth.AND.
     &     (nnzbar.EQ.1).AND.(nnzobs.EQ.1) ) then
CADJ STORE mask2D = tapelev_init, key = 1
        call smooth_hetero2d(localdif,mask2D,
     &     posproc_c(k2smooth),posproc_i(k2smooth),myThid)
        endif
#endif

c--     Compute normalized model-obs cost function
        call ecco_addcost(
     I             localdif, localweight, difmask, nnzobs, dosumsq,
     U             objf_local, num_local,
     I             myThid
     &             )
c--     output model-data difference to disk
        if ( outlev.GT.0 ) then
          il=ilnblnk(outname)
          write(fname3(1:128),'(2a)') 'misfit_', outname(1:il)
          if ( nnzobs.EQ.1 ) CALL
     &    WRITE_REC_XY_RL( fname3, localdif,irec, eccoiter, myThid )
          if ( nnzobs.EQ.nr ) CALL
     &    WRITE_REC_XYZ_RL( fname3, localdif,irec, eccoiter, myThid )
        endif

        endif

      enddo
c--   End of loop over obsfile records.

      call ecco_zero(localdifmeanOut,Nr,zeroRL,myThid)
      call ecco_cp(localdifsum,nnzobs,localdifmeanOut,nnzobs,myThid)
      call ecco_divfield(localdifmeanOut,nnzobs,localdifnum,myThid)
      call ecco_cp(localdifnum,nnzobs,localdifmsk,nnzobs,myThid)
      call ecco_divfield(localdifmsk,nnzobs,localdifnum,myThid)

      if ( domean ) then
c--     Compute normalized model-obs cost function
        call ecco_addcost(
     I      localdifmeanOut, localweight, localdifmsk, nnzobs, dosumsq,
     U      objf_local, num_local, myThid)

c--     output model-data difference to disk
        if ( outlev.GT.0 ) then
          il=ilnblnk(outname)
          write(fname3(1:128),'(2a)') 'misfit_', outname(1:il)
          if ( nnzobs.EQ.1 ) CALL
     &    WRITE_REC_XY_RL(fname3,localdifmeanOut,1,eccoiter,myThid)
          if ( nnzobs.EQ.nr ) CALL
     &    WRITE_REC_XYZ_RL(fname3,localdifmeanOut,1,eccoiter,myThid)
        endif
      endif
      if ( outlev.GT.1 ) then
          il=ilnblnk(outname)
          write(fname3(1:128),'(2a)') 'weight_', outname(1:il)
          if ( nnzobs.EQ.1 ) CALL
     &    WRITE_REC_XY_RL( fname3, localweight,irec, eccoiter, myThid )
          if ( nnzobs.EQ.nr ) CALL
     &    WRITE_REC_XYZ_RL( fname3, localweight,irec, eccoiter, myThid )
      endif

#endif /* ALLOW_ECCO */

      RETURN
      END
