C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_boxmean.F,v 1.1 2014/06/27 14:23:19 gforget Exp $
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
#ifdef ALLOW_COST
# include "ecco_cost.h"
# include "optim.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==
      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer igen_boxmean
      _RL mybar(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mymsk(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpmsk(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      _RL mySumTile(nSx,nSy),myVolTile(nSx,nSy)
      _RL mySumGlo,myVolGlo,myMeanGlo

      _RL tmpSumTile(nSx,nSy),tmpVolTile(nSx,nSy)
      _RL tmpSumGlo,tmpVolGlo,tmpMeanGlo

      integer bi,bj
      integer i,j,k
      integer irec,il
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
      igen_boxmean=0
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'boxmean') igen_boxmean=k
      enddo

      if (igen_boxmean.NE.0) then

c ========

c set bar field params
      doglobalread = .false.
      ladinit      = .false.
      mydummy=xx_tbar_mean_dummy
      il = ilnblnk( tbarfile )
      write(myfname(1:80),'(2a,i10.10)')
     &    tbarfile(1:il),'.',optimcycle

c initialize various things to 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          objf_gencost(bi,bj,igen_boxmean) = 0. _d 0
          num_gencost(bi,bj,igen_boxmean) = 0. _d 0
          mySumTile(bi,bj)=0. _d 0
          myVolTile(bi,bj)=0. _d 0
          mySumGlo=0. _d 0
          myVolGlo=0. _d 0
          do k = 1,nr
          do j = 1,sNy
          do i =  1,sNx
          tmpmsk(i,j,k,bi,bj)=0. _d 0
          enddo
          enddo
          enddo
       ENDDO
      ENDDO

c initialize maximum mask for time series display
      do irec = 1,nmonsrec
        call mdsreadfield( gencost_errfile(igen_boxmean),
     &                     cost_iprec, cost_yftype,
     &                     nr, mymsk, irec, mythid)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        do k = 1,nr
         do j = 1,sNy
          do i =  1,sNx
           if (mymsk(i,j,k,bi,bj).GT.tmpmsk(i,j,k,bi,bj))
     &     tmpmsk(i,j,k,bi,bj)=mymsk(i,j,k,bi,bj)
          enddo
         enddo
        enddo
       enddo
      enddo
      enddo

c ========

c main loop where cost is computed and time series is displayed
      do irec = 1,nmonsrec

c read bar field
        call active_read_xyz( myfname, mybar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        mydummy )

c read mask for averaging (3d, time series)
        call mdsreadfield( gencost_errfile(igen_boxmean),
     &                     cost_iprec, cost_yftype,
     &                     nr, mymsk, irec, mythid)

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
          do k = 1,nr
            do j = 1,sNy
              do i =  1,sNx
c sum that is actually be used in cost function
                mySumTile(bi,bj)=mySumTile(bi,bj)+
     &            mybar(i,j,k,bi,bj)*
     &            hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
     &            *mymsk(i,j,k,bi,bj)
                myVolTile(bi,bj)=myVolTile(bi,bj)+
     &            hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
     &            *mymsk(i,j,k,bi,bj)

c sum for display of time series
                tmpSumTile(bi,bj)=tmpSumTile(bi,bj)+
     &            mybar(i,j,k,bi,bj)*
     &            hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
     &            *tmpmsk(i,j,k,bi,bj)
                tmpVolTile(bi,bj)=tmpVolTile(bi,bj)+
     &            hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
     &            *tmpmsk(i,j,k,bi,bj)
              enddo
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

      WRITE(msgBuf,'(A,I3,1PE21.14)')
     &    'boxmean :',irec,tmpMeanGlo
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

      enddo

c ========


c global sums for cost function
      CALL GLOBAL_SUM_TILE_RL( mySumTile, mySumGlo, myThid )
      CALL GLOBAL_SUM_TILE_RL( myVolTile, myVolGlo, myThid )
      myMeanGlo=0. _d 0
      if ( myVolGlo.GT.0. _d 0)
     &   myMeanGlo=mySumGlo/myVolGlo

      WRITE(msgBuf,'(A,I3,1PE21.14)') 
     &    'boxmean fc :',irec,myMeanGlo
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

c global sum is assigned to master proc,thread
      IF ( MASTER_CPU_THREAD(myThid) ) THEN
      objf_gencost(1,1,igen_boxmean)=myMeanGlo
      num_gencost(1,1,igen_boxmean)=1. _d 0
      ENDIF

c ========

      endif !if (igen_boxmean].NE.0)

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
