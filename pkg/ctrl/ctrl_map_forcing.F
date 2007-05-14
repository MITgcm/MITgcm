
#include "CTRL_CPPOPTIONS.h"

CBOP
C     !ROUTINE: ctrl_map_ini
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_FORCING(myThid)

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE CTRL_MAP_FORCING                              
c     | Add the surface flux anomalies of the control vector
c     | to the model flux fields and update the tile halos.
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
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

      character*( 80)   fnametauu
      character*( 80)   fnametauv
      character*( 80)   fnamesflux
      character*( 80)   fnamehflux
      character*( 80)   fnamesss
      character*( 80)   fnamesst

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==
CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_TAUU0_CONTROL
c--   tauu0.
      il=ilnblnk( xx_tauu_file )
      write(fnametauu(1:80),'(2a,i10.10)')
     &     xx_tauu_file(1:il),'.',optimcycle
      call active_read_xy ( fnametauu, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_tauu_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              fu(i,j,bi,bj) = fu(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_TAUV0_CONTROL
c--   tauv0.
      il=ilnblnk( xx_tauv_file )
      write(fnametauv(1:80),'(2a,i10.10)')
     &     xx_tauv_file(1:il),'.',optimcycle
      call active_read_xy ( fnametauv, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_tauv_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              fv(i,j,bi,bj) = fv(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SFLUX0_CONTROL
c--   sflux0.
      il=ilnblnk( xx_sflux_file )
      write(fnamesflux(1:80),'(2a,i10.10)')
     &     xx_sflux_file(1:il),'.',optimcycle
      call active_read_xy ( fnamesflux, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_sflux_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              empmr(i,j,bi,bj) = empmr(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_HFLUX0_CONTROL
c--   hflux0.
      il=ilnblnk( xx_hflux_file )
      write(fnamehflux(1:80),'(2a,i10.10)')
     &     xx_hflux_file(1:il),'.',optimcycle
      call active_read_xy ( fnamehflux, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_hflux_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              qnet(i,j,bi,bj) = qnet(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SSS_CONTROL
c--   sss0.
      il=ilnblnk( xx_sss_file )
      write(fnamesss(1:80),'(2a,i10.10)')
     &     xx_sss_file(1:il),'.',optimcycle
      call active_read_xy ( fnamesss, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_sss_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              sss(i,j,bi,bj) = sss(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#ifdef ALLOW_SST_CONTROL
c--   sst0.
      il=ilnblnk( xx_sst_file )
      write(fnamesst(1:80),'(2a,i10.10)')
     &     xx_sst_file(1:il),'.',optimcycle
      call active_read_xy ( fnamesst, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      mythid, xx_sst_dummy )
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              sst(i,j,bi,bj) = sst(i,j,bi,bj) + tmpfld2d(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_TAUU0_CONTROL) || defined (ALLOW_TAUV0_CONTROL))
       CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
#endif
#ifdef ALLOW_SFLUX0_CONTROL
       _EXCH_XY_R4(EmPmR, myThid )
#endif
#ifdef ALLOW_HFLUX0_CONTROL
       _EXCH_XY_R4(Qnet,  myThid )
#endif
#ifdef ALLOW_SST_CONTROL
       _EXCH_XY_R4(SST,   myThid )
#endif
#ifdef ALLOW_SSS_CONTROL
       _EXCH_XY_R4(SSS,   myThid )
#endif

      END
