C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_cost_test.F,v 1.3 2013/06/01 15:48:13 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      subroutine thsice_cost_test( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE thsice_cost_test
c     ==================================================================
c
c     o Compute sea-ice cost function.  The following options can be
c       selected with data.thsice (THSICE_PARM02) variable thsice_cost_ice_flag
c
c     thsice_cost_ice_flag = 1
c     - compute mean sea-ice volume
c       costIceStart < mytime < costIceEnd
c
c     thsice_cost_ice_flag = 2
c     - compute mean sea-ice area
c       costIceStart < mytime < costIceEnd
c
c     thsice_cost_ice_flag = 3
c     - heat content of top level plus latent heat of sea-ice
c       costIceStart < mytime < costIceEnd
c
c     thsice_cost_ice_flag = 4
c     - heat content of top level
c       costIceStart < mytime < costIceEnd
c
c     thsice_cost_ice_flag = 5
c     - heat content of top level plus sea-ice plus latent heat of snow
c       costIceStart < mytime < costIceEnd
c
c     thsice_cost_ice_flag = 6
c     - quadratic cost function measuring difference between pkg/thsice
c       AREA variable and simulated sea-ice measurements at every time
c       step.
c
c     ==================================================================
c     SUBROUTINE thsice_cost_test
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_THSICE_COST_TEST
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_COST.h"
#include "THSICE_VARS.h"
#include "DYNVARS.h"
#include "cost.h"
#endif /* ALLOW_THSICE_COST_TEST */

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

#ifdef ALLOW_THSICE_COST_TEST

c     == local variables ==

c     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      integer bi,bj,i,j,kSrf
      _RL tempVar

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( myTime .GT. (endTime - lastinterval) ) then
         tempVar = 1. _d 0/
     &             ( ( 1. _d 0 + min(endTime-startTime,lastinterval) )
     &             / deltaTClock )

         kSrf = 1

         if ( thsice_cost_ice_flag .eq. 1 ) then
c     sea-ice volume
            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_thsice(bi,bj) = objf_thsice(bi,bj) +
     &                       tempVar*rA(i,j,bi,bj)*iceHeight(i,j,bi,bj)
                     enddo
                  enddo
               enddo
            enddo

         elseif ( thsice_cost_ice_flag .eq. 2 ) then
c     sea-ice area
            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_thsice(bi,bj) = objf_thsice(bi,bj) +
     &                       tempVar*rA(i,j,bi,bj)*iceMask(i,j,bi,bj)
                     enddo
                  enddo
               enddo
            enddo

         else
            WRITE(msgBuf,'(A)')
     &           'THSICE_COST_TEST: invalid thsice_cost_ice_flag'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            STOP 'ABNORMAL END: S/R THSICE_COST_TEST'
         endif
      endif

#endif /* ALLOW_THSICE_COST_TEST */

      return
      end
