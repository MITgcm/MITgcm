C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_boxmean.F,v 1.10 2015/11/18 15:30:17 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_boxmean(mythid)

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
# include "ecco.h"
#endif

c     == routine arguments ==
      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer kgen
      _RL mybar(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      _RL mySumTile(nSx,nSy),myVolTile(nSx,nSy)
      _RL mySumGlo,myVolGlo,myMeanGlo

      _RL tmpSumTile(nSx,nSy),tmpVolTile(nSx,nSy)
      _RL tmpSumGlo,tmpVolGlo,tmpMeanGlo

      integer bi,bj
      integer i,j,k
      integer nrecloc,irec,il
      character*(80) myfname
      _RL mydummy
      logical doglobalread
      logical ladinit
      character*(MAX_LEN_MBUF) msgbuf

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

c     == end of interface ==

c-- detect the relevant gencost indices
      kgen=0
      do k=1,NGENCOST
        if ( (gencost_name(k)(1:7).EQ.'boxmean').AND.
     &     (using_gencost(k)) ) kgen=k
      enddo

      if (kgen.NE.0) then

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
     &                        eccoiter, mythid,
     &                        mydummy )
#else
        CALL READ_REC_XY_RL( myfname, mybar,
     &                        iRec, 1, myThid )
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          tmpSumTile(bi,bj)=0. _d 0
          tmpVolTile(bi,bj)=0. _d 0
          tmpSumGlo=0. _d 0
          tmpVolGlo=0. _d 0
        enddo
      enddo

c compute cost
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
            do j = 1,sNy
              do i =  1,sNx
c sum that is actually be used in cost function
      objf_gencost(bi,bj,kgen)=
     &   objf_gencost(bi,bj,kgen)+mybar(i,j,bi,bj)
cgf will need fixing:
      num_gencost(1,1,kgen)= num_gencost(1,1,kgen)
     &   +maskC(i,j,1,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)

c sum for display of time series
      tmpSumTile(bi,bj)=tmpSumTile(bi,bj)+mybar(i,j,bi,bj)
cgf will need fixing:
      tmpVolTile(bi,bj)=tmpVolTile(bi,bj)
     &   +maskC(i,j,1,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)
              enddo
            enddo
        enddo
      enddo

c global sums for display of time series
      CALL GLOBAL_SUM_TILE_RL( tmpSumTile, tmpSumGlo, myThid )
      CALL GLOBAL_SUM_TILE_RL( tmpVolTile, tmpVolGlo, myThid )
      tmpMeanGlo=0. _d 0
      if ( tmpVolGlo.GT.0. _d 0) 
     &   tmpMeanGlo=tmpSumGlo/tmpVolGlo

      WRITE(msgBuf,'(A,I3,A,1PE21.14,A,1PE21.14,A)')
     &    'boxmean :',irec,' ',tmpMeanGlo,' (',tmpVolGlo,')'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

      enddo

c ========


c global sums for cost function
      CALL GLOBAL_SUM_TILE_RL( objf_gencost(1,1,kgen),
     &   mySumGlo, myThid )
      CALL GLOBAL_SUM_TILE_RL( num_gencost(1,1,kgen),
     &   myVolGlo, myThid )
      myMeanGlo=0. _d 0
      if ( myVolGlo.GT.0. _d 0)
     &   myMeanGlo=mySumGlo/myVolGlo

      WRITE(msgBuf,'(A,I3,A,1PE21.14,A,1PE21.14,A)') 
     &    'boxmean fc :',irec,' ',myMeanGlo,' (',myVolGlo,')'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

c ========

      endif !if (kgen].NE.0)

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
