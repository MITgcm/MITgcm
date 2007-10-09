C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_tau_eddy.F,v 1.7 2007/10/09 00:02:51 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_tau_eddy(
     I                           myiter,
     I                           mytime,
     I                           mythid
     &                         )

C     o==========================================================o
C     | subroutine cost_tau_eddy                                 |
C     | o eddy stress cost term                                  |
C     | o now two options:                                       |
C     | ECCO standard or as in Ferreira and Marshall.            |
C     o==========================================================o

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

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

      _RL     locfc
      _RL     tau2_max, tau2_temp

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


#ifdef ALLOW_TAUEDDY_COST_CONTRIBUTION
C------------------------------------------------------
C     Cost function consistent with ECCO standards
C------------------------------------------------------

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

      fcthread = 0. _d 0

      if (optimcycle .ge. 0) then
        ilfld = ilnblnk( xx_edtaux_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_edtaux_file(1:ilfld),'.',optimcycle
      endif
      call active_read_xyz( fnamefld, tmpfld3d, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_edtaux_dummy )
c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Determine the weights to be used.
            fctile = 0. _d 0
            do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfld3d(i,j,k,bi,bj)
#ifndef ALLOW_SMOOTH_CORREL3D
                  fctile = fctile
     &                 + wedtauxFld(i,j,k,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
#else
                  fctile = fctile + tmpx*tmpx
#endif
                endif
              enddo
            enddo
            enddo

            objf_eddytau(bi,bj) = objf_eddytau(bi,bj) + fctile
            fcthread          = fcthread + fctile
          enddo
        enddo

      if (optimcycle .ge. 0) then
        ilfld = ilnblnk( xx_edtauy_file )
        write(fnamefld(1:80),'(2a,i10.10)')
     &       xx_edtauy_file(1:ilfld),'.',optimcycle
      endif
      call active_read_xyz( fnamefld, tmpfld3d, irec, doglobalread,
     &                       ladinit, optimcycle, mythid
     &        , xx_edtauy_dummy )
c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Determine the weights to be used.
            fctile = 0. _d 0
            do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                if (_hFacC(i,j,k,bi,bj) .ne. 0.) then
                  tmpx = tmpfld3d(i,j,k,bi,bj)
#ifndef ALLOW_SMOOTH_CORREL3D
                  fctile = fctile
     &                 + wedtauyFld(i,j,k,bi,bj)*cosphi(i,j,bi,bj)
     &                 *tmpx*tmpx
#else
                  fctile = fctile + tmpx*tmpx
#endif
                endif
              enddo
            enddo
            enddo

            objf_eddytau(bi,bj) = objf_eddytau(bi,bj) + fctile
            fcthread          = fcthread + fctile

#ifdef ECCO_VERBOSE
c--         Print cost function for each tile in each thread.
            write(msgbuf,'(a)') ' '
            call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a,i8.8,1x,i3.3,1x,i3.3)')
     &        ' cost_tau_eddy: irec,bi,bj          =  ',irec,bi,bj
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
        _GLOBAL_SUM_R8( fcthread , myThid )
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




#elif (defined (ALLOW_TAU_EDDY) && defined (ALLOW_COST_TAU_EDDY))
C------------------------------------------------------
C     Cost function as a distance to max. value
C------------------------------------------------------
C
C     maximum autorized value of the Eddy stress (squared)
C     from D. Ferreira
C     values beyond will be penalized;
C     values below are not penalized
      tau2_max = 0.4**2
      locfc = 0.0
c
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c
#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        ikey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
#endif /* ALLOW_AUTODIFF_TAMC */
c
        do j=1,sNy
         do i=1,sNx
          do k=2,Nr
           tau2_temp = (rhonil*Eddytaux(i,j,k,bi,bj))**2
           if ( tau2_temp .gt. tau2_max) then
             locfc = locfc + maskW(i,j,k,bi,bj)*
     &               ( tau2_temp - tau2_max )
           endif
           tau2_temp = (rhonil*Eddytauy(i,j,k,bi,bj))**2
           if ( tau2_temp .gt. tau2_max) then
             locfc = locfc + maskS(i,j,k,bi,bj)*
     &               ( tau2_temp - tau2_max )
           endif
          enddo
         enddo
        enddo
c
        objf_eddytau(bi,bj) = locfc
        print*,'objf_eddytau =',locfc
c
       ENDDO
      ENDDO


#else

      fctile   = 0. _d 0
      fcthread = 0. _d 0

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)')
     &    ' cost_tau_eddy : no contribution to cost function'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

#endif



      return
      end


