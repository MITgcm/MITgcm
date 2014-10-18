C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_theta_ini_fin.F,v 1.7 2014/10/18 18:15:45 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


      subroutine cost_theta_ini_fin(
     I                           myiter,
     I                           mytime,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_theta_ini_fin
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE cost_theta_ini_fin
c     ==================================================================

      implicit none

c     == global variables ==

#ifdef ALLOW_COST_INI_FIN
#include "EEPARAMS.h"
#include "SIZE.h"
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

#ifdef ALLOW_COST_INI_FIN
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

      ilfld = ilnblnk( xx_theta_ini_fin_file )
      write(fnamefld(1:80),'(2a,i10.10)')
     &     xx_theta_ini_fin_file(1:ilfld),'.',optimcycle

      fcthread = 0. _d 0

      call active_read_xyz( fnamefld, tmpfld3d, irec, doglobalread,
     &                    ladinit, optimcycle, mythid,
     &                    xx_theta_ini_fin_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.

            fctile = 0. _d 0
            do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfld3d(i,j,k,bi,bj) - theta(i,j,k,bi,bj)
                  fctile = fctile
     &                 + wtheta(k,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
                endif
              enddo
            enddo
            enddo

            objf_theta_ini_fin(bi,bj) =
     &           objf_theta_ini_fin(bi,bj) + fctile
            fcthread          = fcthread + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_theta_ini_fin: irec,bi,bj          =  ',irec,bi,bj
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,d22.15)')
     &        ' cost_theta_ini_fin: irec,bi,bj          =  ',
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
     &    ' cost_:                       irec =  ',irec
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


