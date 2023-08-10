#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: ctrl_hfacc_ini
C     !INTERFACE:
      subroutine ctrl_hfacc_ini( myThid )

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
      integer myThid

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

      character*(MAX_LEN_FNAM)   fnamehfacc
      character*(max_len_mbuf) msgbuf

      _RL     fac
      _RL     tmpfld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy)
      _RL     tmpfld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      integer ilDir

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLx

      doglobalread = .false.
      ladinit      = .false.

c     Find ctrlDir (w/o trailing blanks) length
      ilDir = ilnblnk(ctrlDir)

      equal = .true.

      if ( equal ) then
        fac = 1. _d 0
      else
        fac = 0. _d 0
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = 1-OLy,sNy+OLy
            do i = 1-OLx,sNx+OLx
              tmpfld2d(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

Cml      write(msgbuf,'(a)')
Cml     &     'ctrl_hfacc_ini: Re-initialising hFacC,'
Cml      call print_message( msgbuf, standardmessageunit,
Cml     &                    SQUEEZE_RIGHT , myThid)
Cml      write(msgbuf,'(a)')
Cml     &     '                adding the control vector.'
Cml      call print_message( msgbuf, standardmessageunit,
Cml     &                    SQUEEZE_RIGHT , myThid)
      write(standardmessageunit,'(21x,a)')
     &     'ctrl_hfacc_ini: Re-initialising hFacC,'
      write(standardmessageunit,'(21x,a)')
     &     '                adding the control vector.'

C     Re-initialize hFacC, so that TAMC/TAF can see it
C     Once hFacC is the control variable, and not its anomaly
C     this will be no longer necessary
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
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
      write(fnamehfacc,'(2a,i10.10)')
     &     ctrlDir(1:ilDir)//xx_hfacc_file(1:il),'.',optimcycle
#ifdef ALLOW_HFACC3D_CONTROL
      call active_read_xyz( fnamehfacc, tmpfld3d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_hfacc_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
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
     &                     myThid, xx_hfacc_dummy )
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

