C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_final.F,v 1.1 2001/03/25 22:33:54 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_Final(
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_Final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_Final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"
#include "ctrl.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

#ifdef ECCO_VERBOSE
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &  ' cost_Final: Evaluating the final cost function.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          print*,' --> objf_temp(bi,bj) =',objf_temp(bi,bj)
          print*,' --> objf_salt(bi,bj) =',objf_salt(bi,bj)
          print*,' --> objf_tauu(bi,bj) =',objf_tauu(bi,bj)
          print*,' --> objf_tauv(bi,bj) =',objf_tauv(bi,bj)
          print*,' --> objf_hflux(bi,bj)   =',objf_hflux(bi,bj)
          print*,' --> objf_sflux(bi,bj)   =',objf_sflux(bi,bj)
          print*,' --> objf_sst(bi,bj)  =',objf_sst(bi,bj)
          print*,' --> objf_h(bi,bj)    =',objf_h(bi,bj)
          print*,' --> objf_atl(bi,bj)  =',objf_atl(bi,bj)
          print*,' --> objf_ctdt(bi,bj) =',objf_ctdt(bi,bj)
          print*,' --> objf_ctds(bi,bj) =',objf_ctds(bi,bj)
          print*,' --> objf_test(bi,bj) =',objf_test(bi,bj)

          fc = fc
     &            + mult_temp * objf_temp(bi,bj)
     &            + mult_salt * objf_salt(bi,bj)
     &            + mult_tauu * objf_tauu(bi,bj)
     &            + mult_tauv * objf_tauv(bi,bj)
     &            + mult_hq   * objf_hflux(bi,bj)  
     &            + mult_hs   * objf_sflux(bi,bj)  
     &            + mult_sst  * objf_sst(bi,bj) 
     &            + mult_h    * objf_h(bi,bj)
     &            + mult_atl  * objf_atl(bi,bj)
     &            + mult_ctdt * objf_ctdt(bi,bj)
     &            + mult_ctds * objf_ctds(bi,bj)
     &            + mult_test * objf_test(bi,bj)
        enddo
      enddo


c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

c--   Each process has calculated the global part for itself.
      _BEGIN_MASTER( mythid )
        fc = fc + mult_hmean*objf_hmean
      _END_MASTER( mythid )
      print*,' --> fc               =',fc

#ifdef ECCO_VERBOSE
      write(msgbuf,'(a,D22.15)')
     &  ' cost_Final: final cost function = ',fc
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &  '             cost function evaluation finished.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

      return
      end

