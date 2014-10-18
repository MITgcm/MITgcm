C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_bottomdrag.F,v 1.9 2014/10/18 18:15:44 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_bottomdrag(
     I                           myiter,
     I                           mytime,
     I                           mythid
     &                         )

C     o==========================================================o
C     | subroutine cost_bottomdrag                               |
C     | o bottom drag adjustment penalization                    |
C     o==========================================================o

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_BOTTOMDRAG_COST_CONTRIBUTION
#include "GRID.h"
#include "DYNVARS.h"

#include "ecco_cost.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#endif
c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_BOTTOMDRAG_COST_CONTRIBUTION

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nrec
      integer irec
      integer ilfld

      _RL fctile
      _RL fcthread
      _RL tmpx

      logical doglobalread
      logical ladinit

      character*(80) fnamefld

      character*(MAX_LEN_MBUF) msgbuf

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Read state record from global file.
      doglobalread = .false.
      ladinit      = .false.

      irec = 1

      if (optimcycle .ge. 0) then
        ilfld = ilnblnk( xx_bottomdrag_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_bottomdrag_file(1:ilfld),'.',optimcycle
      endif

      fcthread = 0. _d 0

      call active_read_xy( fnamefld, tmpfld2d, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_bottomdrag_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.

            fctile = 0. _d 0
            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,1,bi,bj) .ne. 0.) then
                  tmpx = tmpfld2d(i,j,bi,bj)
                IF ( .NOT.ctrlSmoothCorrel2D ) THEN
                  fctile = fctile
     &                 + wbottomdrag(i,j,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
                ELSE !IF ( .NOT.ctrlSmoothCorrel2D ) THEN
                  fctile = fctile + tmpx*tmpx
                ENDIF !IF ( .NOT.ctrlSmoothCorrel2D ) THEN
                endif
              enddo
            enddo

            objf_bottomdrag(bi,bj) = objf_bottomdrag(bi,bj) + fctile
            fcthread          = fcthread + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_bottomdrag: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        '               cost function (dT(0)) = ',
     &        fctile
            call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif
          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_RL( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_bottomdrag:               irec =  ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,d22.15)')
     &    '                 global cost function value = ',
     &    fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

#endif

      return
      end


