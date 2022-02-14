#include "ECCO_OPTIONS.h"

      SUBROUTINE COST_GENCOST_TRANSP( myThid )

C     ==================================================================
C     SUBROUTINE COST_GENCOST_TRANSP
C     ==================================================================
C     o Evaluate cost function contributions of section transport.

      IMPLICIT NONE

C     == global variables ==
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

C     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_GENCOST_CONTRIBUTION
C     == external functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      INTEGER nrecloc, localrec
      INTEGER localstartdate(4)

      _RL myobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL mybar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localdif(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL difmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      _RL dummy, facCost, facNum
      _RL localperiod
      _RL spminloc, spmaxloc, spzeroloc
      _RL tmpMeanTile(nSx,nSy), tmpNumTile(nSx,nSy)
      _RL tmpMeanGlo, tmpNumGlo

      INTEGER kgen(NGENCOST3D)
      INTEGER bi, bj
      INTEGER k
      INTEGER obsrec, irec
      INTEGER il,k2
      INTEGER icount, icount_transp
      LOGICAL dosumsq, dovarwei, doreadobs
      LOGICAL exst

      INTEGER preproc_i(NGENPPROC)
      _RL preproc_r(NGENPPROC)
      CHARACTER*(MAX_LEN_FNAM) mybarfile
      CHARACTER*(MAX_LEN_FNAM) preproc(NGENPPROC)
      CHARACTER*(MAX_LEN_FNAM) preproc_c(NGENPPROC)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(128) fname1, fname0
C     == end of interface ==

C=============== PART 0: initilization ===================

C- facNum is 1 divided by the number of tiles in SIZE dot h
      facNum = nSx*nPx
      facNum = 1. _d 0 / facNum

C-- detect the relevant gencost indices
      do k=1,NGENCOST3D
        kgen(k)=0
      enddo

C-- write a report of how many transport costs
      write(msgbuf,'(A)') 'Inside cost_gencost_transp:'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      icount_transp=0
      do k=1,NGENCOST
        if ( (gencost_name(k)(1:6).EQ.'transp').AND.
     &     (using_gencost(k)) ) then
          icount_transp=icount_transp+1
          kgen(icount_transp)=k
          il=ILNBLNK(gencost_barfile(kgen(icount_transp)))
          write(msgbuf,'(A,i4,A,A)') 'Cost ',kgen(icount_transp),
     &    ': ',gencost_barfile(kgen(icount_transp))(1:il)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

        endif
      enddo

C-- write a report of how many transport costs

      do icount=1,icount_transp
       if (kgen(icount).NE.0) then

C ========

C-- initialize objf and num:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
            objf_gencost(bi,bj,kgen(icount))=0. _d 0
            num_gencost(bi,bj,kgen(icount))=0. _d 0
         ENDDO
        ENDDO

C--   Initialise local variables.
        nrecloc=0
        nrecloc=gencost_nrec(kgen(icount))

C-- only enters if there is at least 1 record
        if(nrecloc.gt.0) then

          facCost = nrecloc
          facCost = 1. _d 0 / facCost

          localperiod=0.
          localperiod=gencost_period(kgen(icount))
          dummy=gencost_dummy(kgen(icount))
          spminloc=gencost_spmin(kgen(icount))
          spmaxloc=gencost_spmax(kgen(icount))
          spzeroloc=gencost_spzero(kgen(icount))

C prefer to have preproc match nosumsq but can not seem to get syntax
C to work for comparison of characters so match dosumsq for now.
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

C-- report of dosumsq flag to make sure it is false
          il=ILNBLNK(gencost_name(kgen(icount)))
          write(msgBuf,'(3A,L5,2A)')
     &    'Cost ',gencost_name(kgen(icount))(1:il),
     &    ' dosumsq: ',dosumsq,' preproc(1): ',preproc(1)(1:7)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

C-- set mybarfile
          mybarfile=gencost_barfile(kgen(icount))

C-- set obsfile if defined. Not use for now. send warning
          doreadobs=.FALSE.
          if( .not. gencost_datafile(kgen(icount)).eq.' ') then
c            doreadobs=.TRUE.
            write(msgBuf,'(A)')
     &      '**WARNING** S/R COST_GENCOST_TRANSP: gencost_datafile '
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            write(msgBuf,'(A)')
     &      'are currently ignored. Adjust the S/R to add code.'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          endif

C model mask[W,S]: already included in transp calc in ecco_phys

C--------- PART 0.1 read weights --------------------

C-- read mask in gencost_mask
C-- for now, assume non-time-variable mask

C=============== PART 1: main loop ===================
          do irec = 1,nrecloc

C--------- PART 1.1 read barfiles ------------------

C-- set all bars to zeros:
            call ecco_zero(mybar,Nr,zeroRL,myThid)

C gencost_mask and fname0 are dummy, get fname1 from mybarfile
            exst=.FALSE.
            call ecco_zero(localtmp,Nr,zeroRL,myThid)
            call cost_gencal(mybarfile,gencost_mask(kgen(icount)),
     &       irec,localstartdate,localperiod,fname1,
     &       fname0,localrec,obsrec,exst,myThid)
            call cost_genread(fname1,mybar,localtmp,irec,Nr,Nr,
     &       nrecloc,preproc,preproc_c,preproc_i,preproc_r,
     &       dummy,myThid)

C--------- PART 1.2 read data --------------------

C-- ignore for now, but use doreadobs flag if needed
C-- be careful of recomputation when put inside if-end block
c            if(doreadobs) then
            call ecco_zero(myobs,Nr,zeroRL,myThid)
c            endif

C--------- PART 1.3 Cost calculation -------------

C-- keep total at each irec to print out for time-series
            DO bj = myByLo(myThid), myByHi(myThid)
             DO bi = myBxLo(myThid), myBxHi(myThid)
               tmpMeanTile(bi,bj) = 0. _d 0
               tmpNumTile(bi,bj) = 0. _d 0
             ENDDO
            ENDDO

C compute obs minus bar (localdif) and mask (difmask)
C note localtmp is set to 1.
            call ecco_zero(localtmp,Nr,oneRL,myThid)
            call ecco_zero(localdif,Nr,zeroRL,myThid)
            call ecco_zero(difmask,Nr,zeroRL,myThid)

C take care to set sp[min,max,zero]loc carefully to not
C filter out signal.  Can consider skip diffmsk step
            call ecco_diffmsk(
     I       mybar, myobs, localtmp,
     I       Nr, Nr,spminloc, spmaxloc, spzeroloc,
     O       localdif, difmask,
     I       myThid )

            call ecco_zero(localtmp,Nr,oneRL,myThid)
            call ecco_addcost(
     I       localdif,localtmp,difmask,Nr,Nr,dosumsq,
     O       tmpMeanTile,tmpNumTile,
     I       myThid)

cC either use ecco_diffmsk and ecco_addcost from above or simplify
cC to call below. For now keep syntax consistent with gencost
c            call ecco_zero(localtmp,Nr,oneRL,myThid)
c            call ecco_addcost(
c     I       mybar,localtmp,localtmp,Nr,Nr,dosumsq,
c     O       tmpMeanTile,tmpNumTile,
c     I       myThid)

C global sums for display of time series
C note tmpNumGlo is the constant total wet points in the gencost_mask[W,S]
            tmpMeanGlo = 0. _d 0
            tmpNumGlo = 0. _d 0
            il=ILNBLNK(gencost_barfile(kgen(icount)))
            CALL GLOBAL_SUM_TILE_RL( tmpMeanTile, tmpMeanGlo, myThid )
            CALL GLOBAL_SUM_TILE_RL( tmpNumTile, tmpNumGlo, myThid )
            WRITE(msgBuf,'(2A,I3,A,1PE21.14,1PE21.14)')
     &        'globalsum transp ',gencost_barfile(kgen(icount))(1:il),
     &        irec,' ',tmpMeanGlo,tmpNumGlo
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )

C sum that is actually be used in cost function
            DO bj = myByLo(myThid), myByHi(myThid)
             DO bi = myBxLo(myThid), myBxHi(myThid)
               objf_gencost(bi,bj,kgen(icount))=
     &            objf_gencost(bi,bj,kgen(icount))+tmpMeanTile(bi,bj)
             ENDDO
            ENDDO

          enddo !irec

C-- last step:
C-- divide by number of record to get mean transport:
C-- make num_gencost equals number of months/days used
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             objf_gencost(bi,bj,kgen(icount))=
     &          objf_gencost(bi,bj,kgen(icount))*facCost
             num_gencost(bi,bj,kgen(icount))=nrecloc*facNum
           ENDDO
          ENDDO

        endif !if (nrecloc.gt.0)
       endif !if (kgen.NE.0)
      enddo !icount_transp

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      END
