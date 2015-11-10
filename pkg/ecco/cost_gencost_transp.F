C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_transp.F,v 1.2 2015/11/10 13:58:14 atn Exp $
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

      _RL myobs     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mybar     (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localdif  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL difmask   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localweight(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localtmp   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      _RL dummy,fac
      _RL localperiod
      _RL spminloc, spmaxloc, spzeroloc

      _RL tmpMeanTile(nSx,nSy),tmpNumTile(nSx,nSy)
      _RL tmpMeanGlo,tmpNumGlo

      character*(MAX_LEN_FNAM) mybarfile
      character*(MAX_LEN_FNAM) myobsfile

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
      character*(128) fname1, fname0

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

c-- set mybarfile
          mybarfile=gencost_barfile(kgen)

c-- write some diagnostics of actual files opened here:
          write(msgbuf,'(A)') 'Inside cost_gencost_transp: '
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

c-- set obsfile if defined
          doreadobs=.FALSE.
          if( .not. gencost_datafile(kgen).eq.' ') doreadobs=.TRUE.

c model mask[W,S], 
c note calc transp in ecco_phys have already include mask[W,S]

c====================================================
c--------- PART 0.1 read weights --------------------
c====================================================
c-- store mask in errfile, already stored in ecco.h under msktrVol[W,S]
c-- for now, assume non-time-variable mask:

c=============== PART 1: main loop ===================
          do irec = 1,nrecloc
c====================================================
c--------- PART 1.1 read barfiles ------------------
c====================================================
c-- set all bars to zeros:
            call ecco_zero(mybar,Nr,zeroRL,myThid)

c gencost_errfile and fname0 are dummy, get fname0 from mybarfile
          write(msgbuf,'(A)') 'pre- read mybarfile'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
            exst=.FALSE.
            call ecco_zero(localtmp,Nr,zeroRL,myThid)
            call cost_gencal(mybarfile,gencost_errfile(kgen),
     &       irec,localstartdate,localperiod,fname1,
     &       fname0,localrec,obsrec,exst,mythid)
          il=ilnblnk(fname1)
          write(msgbuf,'(2A)') 'mybarfile:',fname1(1:il)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
            call cost_genread(fname1,mybar,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,mythid)
          write(msgbuf,'(A)') 'post- read mybarfile'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

c====================================================
c--------- PART 1.2 read data --------------------
c====================================================
c-- ignore for now, but use doreadobs flag if needed
c            if(doreadobs) then
            call ecco_zero(myobs,Nr,zeroRL,myThid)
c            endif

c====================================================
c--------- PART 1.3 Cost calculation -------------
c====================================================

c-- keep total at each irec to print out for time-series
            DO bj = jtlo,jthi
              DO bi = itlo,ithi
                tmpMeanTile(bi,bj) = 0. _d 0
                tmpNumTile(bi,bj) = 0. _d 0
              ENDDO
            ENDDO

c compute obs minus bar (localdif) and mask (difmask) 
c note localtmp is set to 1.
            call ecco_zero(localtmp,nnzobs,oneRL,mythid)
            call ecco_zero(localdif,nnzobs,zeroRL,mythid)
            call ecco_zero(difmask,nnzobs,zeroRL,mythid)

            call ecco_diffmsk(
     I       mybar, nnzbar, myobs, nnzobs, localtmp,
     I       spminloc, spmaxloc, spzeroloc,
     O       localdif, difmask,
     I       myThid )

            call ecco_zero(localtmp,nnzobs,oneRL,mythid)
            call ecco_addcost(
     I       localdif,localtmp,difmask,nnzobs,dosumsq,
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
