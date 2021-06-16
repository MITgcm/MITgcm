#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: ctrl_hfacc_ini
C     !INTERFACE:
      subroutine ctrl_hfacc_ini( mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_hfacc_ini
c     | Add the hFacC part of the control vector to the model state
c     | and update the tile halos.
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid

#ifdef ALLOW_HFACC_CONTROL
C     !LOCAL VARIABLES:
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il

      logical equal
      logical doglobalread
      logical ladinit

      character*( 80)   fnamehfacc
      character*(max_len_mbuf) msgbuf

      _RL     fac
      _RL     tmpfld2d(1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL     tmpfld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      doglobalread = .false.
      ladinit      = .false.

      equal = .true.

      if ( equal ) then
        fac = 1. _d 0
      else
        fac = 0. _d 0
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1-oly,sny+oly
            do i = 1-olx,snx+olx
              tmpfld2d(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

Cml      write(msgbuf,'(a)')
Cml     &     'ctrl_hfacc_ini: Re-initialising hFacC,'
Cml      call print_message( msgbuf, standardmessageunit,
Cml     &                    SQUEEZE_RIGHT , mythid)
Cml      write(msgbuf,'(a)')
Cml     &     '                adding the control vector.'
Cml      call print_message( msgbuf, standardmessageunit,
Cml     &                    SQUEEZE_RIGHT , mythid)
      write(standardmessageunit,'(21x,a)')
     &     'ctrl_hfacc_ini: Re-initialising hFacC,'
      write(standardmessageunit,'(21x,a)')
     &     '                adding the control vector.'

C     Re-initialize hFacC, so that TAMC/TAF can see it
C     Once hFacC is the control variable, and not its anomaly
C     this will be no longer necessary
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                hFacC(i,j,k,bi,bj)    = 0.
                tmpfld3d(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
       enddo
      enddo
      _BEGIN_MASTER( myThid )
      CALL READ_FLD_XYZ_RL( 'hFacC', ' ', hFacC, 0, myThid )
      _END_MASTER( myThid )
      _EXCH_XYZ_RS( hFacC ,myThid )

C--
      il=ilnblnk( xx_hfacc_file )
      write(fnamehfacc(1:80),'(2a,i10.10)')
     &     xx_hfacc_file(1:il),'.',optimcycle
#ifdef ALLOW_HFACC3D_CONTROL
      call active_read_xyz( fnamehfacc, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_hfacc_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                hFacC(i,j,k,bi,bj) = hFacC(i,j,k,bi,bj) +
     &                               fac*tmpfld3d(i,j,k,bi,bj)
              enddo
            enddo
          enddo
       enddo
      enddo
#else /* ALLOW_HFACC3D_CONTROL undefined */
      call active_read_xy( fnamehfacc, tmpfld2d, 1,
     &                     doglobalread, ladinit, optimcycle,
     &                     mythid, xx_hfacc_dummy )
      do bj = jtlo,jthi
         do bi = itlo,ithi
            do j = jmin,jmax
               do i = imin,imax
                  k = k_lowC(i,j,bi,bj)
c                  if ( k .gt. 0 ) then
                     hFacC(i,j,k,bi,bj) = hFacC(i,j,k,bi,bj)
     &                                  + fac*tmpfld2d(i,j,bi,bj)
c                  end if
               enddo
            enddo
         enddo
      enddo
#endif /* ALLOW_HFACC3D_CONTROL */

c--   Update the tile edges.

      CALL dummy_in_hfac( 'C', 0, myThid )
      _EXCH_XYZ_RS( hFacC, myThid )
      CALL dummy_in_hfac( 'C', 1, myThid )

#endif /* ALLOW_HFACC_CONTROL */

      return
      end

