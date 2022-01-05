#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_boxmean( myThid )

c     ==================================================================
c     SUBROUTINE cost_gencost_boxmean
c     ==================================================================
c
c     o Evaluate cost function contributions of box mean THETA.
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
      integer myThid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer kgen
      _RL mybar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL mySumTile(nSx,nSy),myVolTile(nSx,nSy)
      _RL mySumGlo,myVolGlo

      _RL tmpSumTile(nSx,nSy)
      _RL tmpSumGlo

      integer bi,bj
      integer i,j
      integer nrecloc,irec,il,ioUnit
      character*(80) myfname
      _RL mydummy
      logical doglobalread
      logical ladinit
      character*(MAX_LEN_MBUF) msgbuf

      LOGICAL exst
      CHARACTER*(128) tempfile
      _RS     dummyRS(1)
      _RL     gencost_mskTemporal
      _RL     tmpVar(1)

c     == external functions ==
      integer  ilnblnk
      external ilnblnk
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

c     == end of interface ==

c-- detect the relevant gencost indices
      do kgen=1,NGENCOST
        if ( (gencost_flag(kgen).EQ.-3).AND.(using_gencost(kgen)) ) then

c ========

c set bar field params
      doglobalread = .false.
      ladinit      = .false.
      mydummy=gencost_dummy(kgen)
      il = ilnblnk( gencost_barfile(kgen) )
      write(myfname(1:80),'(2a,i10.10)')
     &    gencost_barfile(kgen)(1:il),'.',eccoiter

c initialize various things to 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          mySumTile(bi,bj)=0. _d 0
          myVolTile(bi,bj)=0. _d 0
          mySumGlo=0. _d 0
          myVolGlo=0. _d 0
       ENDDO
      ENDDO

      nrecloc=gencost_nrec(kgen)

c ========

c main loop where cost is computed and time series is displayed
      do irec = 1,nrecloc

c read bar field
#ifdef ALLOW_AUTODIFF
        call active_read_xy( myfname, mybar, irec,
     &                        doglobalread, ladinit,
     &                        eccoiter, myThid,
     &                        mydummy )
#else
        CALL READ_REC_XY_RL( myfname, mybar,
     &                        iRec, 1, myThid )
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          tmpSumTile(bi,bj)=0. _d 0
          tmpSumGlo=0. _d 0
        enddo
      enddo

      il = ilnblnk(gencost_mask(kgen))
      write(tempfile(1:128),'(2A)') gencost_mask(kgen)(1:il),'T'
      inquire( file=tempfile(1:il+1), exist=exst )
      if ( (.NOT.exst).OR.(gencost_mask(kgen).EQ.' ') ) then
       gencost_mskTemporal=nrecloc
       gencost_mskTemporal=1. _d 0 / gencost_mskTemporal
      else
       ioUnit = 0
       call MDS_READVEC_LOC(tempfile,cost_iprec,ioUnit,'RL',
     &      1, tmpVar, dummyRS, 0, 0, iRec, myThid )
       gencost_mskTemporal = tmpVar(1)
      endif

c compute cost
      IF ( myProcId .EQ. 0 ) num_gencost(1,1,kgen)=
     &   num_gencost(1,1,kgen)+gencost_mskTemporal

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
            do j = 1,sNy
              do i =  1,sNx
c sum that is actually be used in cost function
      objf_gencost(bi,bj,kgen)=objf_gencost(bi,bj,kgen)
     &   +gencost_mskTemporal*mybar(i,j,bi,bj)

c sum for display of time series
      tmpSumTile(bi,bj)=tmpSumTile(bi,bj)
     &   +gencost_mskTemporal*mybar(i,j,bi,bj)
              enddo
            enddo
        enddo
      enddo

c global sums for display of time series
      CALL GLOBAL_SUM_TILE_RL( tmpSumTile, tmpSumGlo, myThid )

      WRITE(msgBuf,'(A,I3,A,1PE21.14)')
     &    'boxmean/horflux :',irec,' ',tmpSumGlo
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

      enddo

c ========

c global sums for cost function
      CALL GLOBAL_SUM_TILE_RL( objf_gencost(1,1,kgen),
     &   mySumGlo, myThid )

      WRITE(msgBuf,'(A,1PE21.14)') 'boxmean/horflux fc :',mySumGlo
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

c ========

      endif !if ( (gencost_flag(kgen).EQ.-3).AND.
      enddo !do kgen=1,NGENCOST

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      return
      end
