C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_transp.F,v 1.1 2015/11/10 08:08:42 atn Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_transp(mythid)

c     ==================================================================
c     SUBROUTINE cost_gencost_transp
c     ==================================================================
c
c     o Evaluate cost function contributions of section transport.
c
c     ==================================================================
c     SUBROUTINE cost_gencost_boxmean
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

c     == routine arguments ==
      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
#ifdef ALLOW_GENCOST_TRANSPORT

c     == local variables ==

      integer nnzobs, nnzbar
      parameter (nnzbar = Nr, nnzobs = Nr)
      integer nrecloc, localrec
      integer localstartdate(4)

c      _RL modelmskS(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
c      _RL modelmskW(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL myobsS     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL myobsW     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mybarS     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mybarW     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mymskS     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mymskW     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localdifW  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localdifS  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL difmaskW   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL difmaskS   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localweightS(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localweightW(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localtmp   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      _RL dummy,fac
      _RL localperiod
      _RL spminloc, spmaxloc, spzeroloc

      _RL tmpMeanTile(nSx,nSy),tmpNumTile(nSx,nSy)
      _RL tmpMeanGlo,tmpNumGlo

      character*(MAX_LEN_FNAM) mybarfileW,mybarfileS
      character*(MAX_LEN_FNAM) myobsfileW,myobsfileS
      character*(MAX_LEN_FNAM) mymskfileW,mymskfileS

      integer kgen
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer obsrec,irec,jrec
      integer il,k2
      logical dosumsq, dovarwei, doreadobs

      integer preproc_i(NGENPPROC)
      _RL preproc_r(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)


      logical doglobalread
      logical ladinit
      character*(MAX_LEN_MBUF) msgbuf
      character*(128) fnameS,fnameW,fname0

      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c=============== PART 0: initilization ===================

c-- detect the relevant gencost indices
      kgen=0
      do k=1,NGENCOST
        if ( (gencost_name(k)(1:6).EQ.'transp').AND.
     &     (using_gencost(k)) ) kgen=k
      enddo

      if (kgen.NE.0) then

c ========

c-- initialize objf and num:
        DO bj=jtlo,jthi
         DO bi=itlo,ithi
            objf_gencost(bi,bj,kgen)=0. _d 0
            num_gencost(bi,bj,kgen)=0. _d 0
         ENDDO
        ENDDO

c--   Initialise local variables.
        nrecloc=0
        nrecloc=gencost_nrec(kgen)

c-- only enters if there is at least 1 record 
        if(nrecloc.gt.0) then

          fac=1. _d 0 / nrecloc

          localperiod=0.
          localperiod=gencost_period(kgen)
          dummy=gencost_dummy(kgen)
          spminloc=gencost_spmin(kgen)
          spmaxloc=gencost_spmax(kgen)
          spzeroloc=gencost_spzero(kgen)


          dosumsq=.FALSE.
          dovarwei=.FALSE.
          do k2 = 1, NGENPPROC
            preproc(k2)=gencost_preproc(k2,kgen)
            preproc_i(k2)=gencost_preproc_i(k2,kgen)
            preproc_c(k2)=gencost_preproc_c(k2,kgen)
            preproc_r(k2)=gencost_preproc_r(k2,kgen)
            if (preproc(k2).EQ.'variaweight') dovarwei=.TRUE.
            if (.not.(preproc(k2).EQ.'nosumsq')) dosumsq=.TRUE.
          enddo

c-- write some diagnostics of actual files opened here:
c          write(msgbuf,'(A)') 'Inside cost_gencost_transp: '
c          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c     &                        SQUEEZE_RIGHT, myThid )

c-- set barfile
          il=ilnblnk(gencost_barfile(kgen))
          write(mybarfileW(1:MAX_LEN_FNAM),'(2A)')
     &        gencost_barfile(kgen)(1:il),'W'
          write(mybarfileS(1:MAX_LEN_FNAM),'(2A)')
     &        gencost_barfile(kgen)(1:il),'S'

c-- set obsfile if defined
          doreadobs=.FALSE.
          if( .not. gencost_datafile(kgen).eq.' ') doreadobs=.TRUE.
          if( doreadobs ) then
            il=ilnblnk(gencost_datafile(kgen))
            write(myobsfileW(1:MAX_LEN_FNAM),'(2A)')
     &        gencost_datafile(kgen)(1:il),'W'
            write(myobsfileS(1:MAX_LEN_FNAM),'(2A)')
     &        gencost_datafile(kgen)(1:il),'S'
          endif !doreadobs

c-- store mask in errfile
          il=ilnblnk(gencost_errfile(kgen))
          write(mymskfileW(1:MAX_LEN_FNAM),'(2A)') 
     &        gencost_errfile(kgen)(1:il),'W'
          write(mymskfileS(1:MAX_LEN_FNAM),'(2A)') 
     &        gencost_errfile(kgen)(1:il),'S'

c initialize obs to zeros if not read in obs
          if(.not. doreadobs) then
            call ecco_zero(myobsW,Nr,zeroRL,myThid)
            call ecco_zero(myobsS,Nr,zeroRL,myThid)
          endif

c model mask[W,S], 
c note calc transp in ecco_phys have already include mask[W,S]
c          call ecco_zero(modelmskW,Nr,zeroRL,myThid)
c          call ecco_zero(modelmskS,Nr,zeroRL,myThid)
c          call ecco_cprsrl(maskW,Nr,modelmskW,Nr,myThid)
c          call ecco_cprsrl(maskS,Nr,modelmskS,Nr,myThid)

c====================================================
c--------- PART 0.1 read weights --------------------
c====================================================
c-- for now, assume non-time-variable mask:
          call ecco_zero(mymskW,1,zeroRL,myThid)
          call ecco_zero(mymskS,1,zeroRL,myThid)
          call ecco_readwei(mymskfileW,mymskW,1,Nr,myThid)
          call ecco_readwei(mymskfileS,mymskS,1,Nr,myThid)

c-- get a mask of where to count the data points
c-- these two fields localweight[W,S] will be used in ecco_addcost
c-- for the purpose of counting number of obs.  Right now they do
c-- not have a real meaning as a transport per irec is 1 record,
c-- but works for syntax in ecco_addcost: junk*localweight
c-- localweight is binary 0/1
          call ecco_zero(localtmp,Nr,zeroRL,myThid)
          call ecco_zero(localweightW,Nr,zeroRL,myThid)
          call ecco_addmask(localtmp,mymskW,Nr,localtmp,localweightW,
     &                      Nr,myThid)

          call ecco_zero(localtmp,Nr,zeroRL,myThid)
          call ecco_zero(localweightS,Nr,zeroRL,myThid)
          call ecco_addmask(localtmp,mymskS,Nr,localtmp,localweightS,
     &                       Nr,myThid)

c=============== PART 1: main loop ===================
          do irec = 1,nrecloc
c====================================================
c--------- PART 1.1 read barfiles ------------------
c====================================================
c-- set all bars to zeros:
            call ecco_zero(mybarW,Nr,zeroRL,myThid)
            call ecco_zero(mybarS,Nr,zeroRL,myThid)

c-- 1.1.A West
c mymskfileW and fname0 are dummy, get fnameW from mybarfileW
            exst=.FALSE.
            call ecco_zero(localtmp,Nr,zeroRL,myThid)
            call cost_gencal(mybarfileW,mymskfileW,
     &       irec,localstartdate,localperiod,fnameW,
     &       fname0,localrec,obsrec,exst,mythid)
            call cost_genread(fnameW,mybarW,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,mythid)

c-- 1.1.B South
c mymskfileS and fname0 are dummy, get fnameS from mybarfileS
            exst=.FALSE.
            call ecco_zero(localtmp,Nr,zeroRL,myThid)
            call cost_gencal(mybarfileS,mymskfileS,
     &       irec,localstartdate,localperiod,fnameS,
     &       fname0,localrec,obsrec,exst,mythid)
            call cost_genread(fnameS,mybarS,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,mythid)

c====================================================
c--------- PART 1.2 read data --------------------
c====================================================
c-- ignore for now, but use doreadobs flag if needed

c====================================================
c--------- PART 1.3 Transport Mask ---------------
c====================================================
c-- note that can take care of mask here or in ecco_addcost
c-- however, if/when obs is avail to read in, best to take care here.
c-- mybar[W,S] are updated on output
            call ecco_multfield(mymskW,nnzbar,mybarW,mythid)
            call ecco_multfield(mymskS,nnzbar,mybarS,mythid)
            if(doreadobs) then
              call ecco_multfield(mymskW,nnzbar,myobsW,mythid)
              call ecco_multfield(mymskS,nnzbar,myobsS,mythid)
            endif

c====================================================
c--------- PART 1.4 Cost calculation -------------
c====================================================

c-- keep total at each irec to print out for time-series
            DO bj = jtlo,jthi
              DO bi = itlo,ithi
                tmpMeanTile(bi,bj) = 0. _d 0
                tmpNumTile(bi,bj) = 0. _d 0
              ENDDO
            ENDDO

c compute obs minus bar (localdif) and mask (difmask) 
            call ecco_zero(localtmp,nnzobs,oneRL,mythid)
            call ecco_zero(localdifW,nnzobs,zeroRL,mythid)
            call ecco_zero(localdifS,nnzobs,zeroRL,mythid)
            call ecco_zero(difmaskW,nnzobs,zeroRL,mythid)
            call ecco_zero(difmaskS,nnzobs,zeroRL,mythid)

            call ecco_diffmsk(
     I       mybarW, nnzbar, myobsW, nnzobs, localtmp,
     I       spminloc, spmaxloc, spzeroloc,
     O       localdifW, difmaskW,
     I       myThid )
            call ecco_diffmsk(
     I       mybarS, nnzbar, myobsS, nnzobs, localtmp,
     I       spminloc, spmaxloc, spzeroloc,
     O       localdifS, difmaskS,
     I       myThid )

            call ecco_addcost(
     I       localdifW,localweightW,difmaskW,nnzobs,dosumsq,
     O       tmpMeanTile,tmpNumTile,
     I       mythid)
            call ecco_addcost(
     I       localdifS,localweightS,difmaskS,nnzobs,dosumsq,
     O       tmpMeanTile,tmpNumTile,
     I       mythid)

c global sums for display of time series
            tmpMeanGlo = 0. _d 0
            tmpNumGlo = 0. _d 0
            il=ilnblnk(gencost_barfile(kgen))
            CALL GLOBAL_SUM_TILE_RL( tmpMeanTile, tmpMeanGlo, myThid )
            CALL GLOBAL_SUM_TILE_RL( tmpNumTile, tmpNumGlo, myThid )
            WRITE(msgBuf,'(2A,I3,1PE21.14,1PE21.14)')
     &        'transp : ',gencost_barfile(kgen)(1:il),
     &        irec,tmpMeanGlo,tmpNumGlo
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )

c sum that is actually be used in cost function
            DO bj = jtlo,jthi
              DO bi = itlo,ithi
                objf_gencost(bi,bj,kgen)=
     &             objf_gencost(bi,bj,kgen)+tmpMeanTile(bi,bj)
              ENDDO
            ENDDO

          enddo !irec

c-- last step: 
c-- divide by number of record to get mean transport:
c-- make num_gencost equals number of months/days used
          do bj = jtlo,jthi
            do bi = itlo,ithi
              objf_gencost(bi,bj,kgen)=objf_gencost(bi,bj,kgen)*fac
              num_gencost(bi,bj,kgen)=nrecloc
            enddo
          enddo

        endif !if (nrecloc.gt.0)
      endif !if (kgen.NE.0)

#endif /* ALLOW_GENCOST_TRANSPORT */
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      end
