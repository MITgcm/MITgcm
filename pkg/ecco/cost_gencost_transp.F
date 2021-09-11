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
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION

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

      _RL dummy,facCost,facNum
c- facNum is 1 divided by the number of tiles in SIZE dot h
      parameter  ( facNum = 1./float(nPx) )
      _RL localperiod
      _RL spminloc, spmaxloc, spzeroloc

      _RL tmpMeanTile(nSx,nSy),tmpNumTile(nSx,nSy)
      _RL tmpMeanGlo,tmpNumGlo

      character*(MAX_LEN_FNAM) mybarfile
      character*(MAX_LEN_FNAM) myobsfile

      integer kgen(NGENCOST3D)
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer obsrec,irec,jrec
      integer il,k2
      integer icount,icount_transp
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
      do k=1,NGENCOST3D
        kgen(k)=0
      enddo

c-- write a report of how many transport costs
      write(msgbuf,'(A)') 'Inside cost_gencost_transp:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      icount_transp=0
      do k=1,NGENCOST
        if ( (gencost_name(k)(1:6).EQ.'transp').AND.
     &     (using_gencost(k)) ) then
          icount_transp=icount_transp+1
          kgen(icount_transp)=k
          il=ilnblnk(gencost_barfile(kgen(icount_transp)))
          write(msgbuf,'(A,i4,A,A)') 'Cost ',kgen(icount_transp),
     &    ': ',gencost_barfile(kgen(icount_transp))(1:il)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

        endif
      enddo

c-- write a report of how many transport costs

      do icount=1,icount_transp
      if (kgen(icount).NE.0) then

c ========

c-- initialize objf and num:
        DO bj=jtlo,jthi
         DO bi=itlo,ithi
            objf_gencost(bi,bj,kgen(icount))=0. _d 0
            num_gencost(bi,bj,kgen(icount))=0. _d 0
         ENDDO
        ENDDO

c--   Initialise local variables.
        nrecloc=0
        nrecloc=gencost_nrec(kgen(icount))

c-- only enters if there is at least 1 record
        if(nrecloc.gt.0) then

          facCost=1. _d 0 / float(nrecloc)

          localperiod=0.
          localperiod=gencost_period(kgen(icount))
          dummy=gencost_dummy(kgen(icount))
          spminloc=gencost_spmin(kgen(icount))
          spmaxloc=gencost_spmax(kgen(icount))
          spzeroloc=gencost_spzero(kgen(icount))

c prefer to have preproc match nosumsq but can not seem to get syntax
c to work for comparison of characters so match dosumsq for now.
          dosumsq=.FALSE.
          dovarwei=.FALSE.
          do k2 = 1, NGENPPROC
            preproc(k2)=gencost_preproc(k2,kgen(icount))
            preproc_i(k2)=gencost_preproc_i(k2,kgen(icount))
            preproc_c(k2)=gencost_preproc_c(k2,kgen(icount))
            preproc_r(k2)=gencost_preproc_r(k2,kgen(icount))
            if (preproc(k2).EQ.'variaweight') dovarwei=.TRUE.
            if (preproc(k2)(1:7).EQ.'dosumsq') dosumsq=.TRUE.
          enddo

c-- report of dosumsq flag to make sure it is false
          il=ilnblnk(gencost_name(kgen(icount)))
          write(msgbuf,'(3A,L5,2A)')
     &    'Cost ',gencost_name(kgen(icount))(1:il),
     &    ' dosumsq: ',dosumsq,' preproc(1): ',preproc(1)(1:7)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

c-- set mybarfile
          mybarfile=gencost_barfile(kgen(icount))

c-- set obsfile if defined. Not use for now. send warning
          doreadobs=.FALSE.
          if( .not. gencost_datafile(kgen(icount)).eq.' ') then
c            doreadobs=.TRUE.
            write(msgbuf,'(A)')
     &      '**WARNING** S/R COST_GENCOST_TRANSP: gencost_datafile '
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            write(msgbuf,'(A)')
     &      'are currently ignored. Adjust the S/R to add code.'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          endif

c model mask[W,S]: already included in transp calc in ecco_phys

c--------- PART 0.1 read weights --------------------

c-- read mask in gencost_mask
c-- for now, assume non-time-variable mask

c=============== PART 1: main loop ===================
          do irec = 1,nrecloc

c--------- PART 1.1 read barfiles ------------------

c-- set all bars to zeros:
            call ecco_zero(mybar,Nr,zeroRL,myThid)

c gencost_mask and fname0 are dummy, get fname1 from mybarfile
            exst=.FALSE.
            call ecco_zero(localtmp,Nr,zeroRL,myThid)
            call cost_gencal(mybarfile,gencost_mask(kgen(icount)),
     &       irec,localstartdate,localperiod,fname1,
     &       fname0,localrec,obsrec,exst,mythid)
            call cost_genread(fname1,mybar,localtmp,irec,nnzbar,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,mythid)

c--------- PART 1.2 read data --------------------

c-- ignore for now, but use doreadobs flag if needed
c-- be careful of recomputation when put inside if-end block
c            if(doreadobs) then
            call ecco_zero(myobs,Nr,zeroRL,myThid)
c            endif

c--------- PART 1.3 Cost calculation -------------

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

c take care to set sp[min,max,zero]loc carefully to not
c filter out signal.  Can consider skip diffmsk step
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

cc either use ecco_diffmsk and ecco_addcost from above or simplify
cc to call below. For now keep syntax consistent with gencost
c            call ecco_zero(localtmp,nnzobs,oneRL,mythid)
c            call ecco_addcost(
c     I       mybar,localtmp,localtmp,nnzobs,dosumsq,
c     O       tmpMeanTile,tmpNumTile,
c     I       mythid)

c global sums for display of time series
c note tmpNumGlo is the constant total wet points in the gencost_mask[W,S]
            tmpMeanGlo = 0. _d 0
            tmpNumGlo = 0. _d 0
            il=ilnblnk(gencost_barfile(kgen(icount)))
            CALL GLOBAL_SUM_TILE_RL( tmpMeanTile, tmpMeanGlo, myThid )
            CALL GLOBAL_SUM_TILE_RL( tmpNumTile, tmpNumGlo, myThid )
            WRITE(msgBuf,'(2A,I3,A,1PE21.14,1PE21.14)')
     &        'globalsum transp ',gencost_barfile(kgen(icount))(1:il),
     &        irec,' ',tmpMeanGlo,tmpNumGlo
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )

c sum that is actually be used in cost function
            DO bj = jtlo,jthi
              DO bi = itlo,ithi
                objf_gencost(bi,bj,kgen(icount))=
     &             objf_gencost(bi,bj,kgen(icount))+tmpMeanTile(bi,bj)
              ENDDO
            ENDDO

          enddo !irec

c-- last step:
c-- divide by number of record to get mean transport:
c-- make num_gencost equals number of months/days used
          do bj = jtlo,jthi
            do bi = itlo,ithi
              objf_gencost(bi,bj,kgen(icount))=
     &           objf_gencost(bi,bj,kgen(icount))*facCost
              num_gencost(bi,bj,kgen(icount))=nrecloc*facNum
            enddo
          enddo

        endif !if (nrecloc.gt.0)
      endif !if (kgen.NE.0)
      enddo !icount_transp

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      end
