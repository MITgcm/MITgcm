C $Header: /u/gcmpack/MITgcm/pkg/streamice/adstreamice_invert_surf_forthick.F,v 1.4 2014/07/09 16:09:48 dgoldberg Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#include "STREAMICE_OPTIONS.h"

      SUBROUTINE ADSTRMICE_H_INV (myThid)

!      This S/R finds a thickness (H) that gives surf elev S with bed R

#include "SIZE.h"
#include "GRID.h"
#include "SET_GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
!#include "CTRL_GENARR.h"

      INTEGER myThid

#ifdef ALLOW_STREAMICE

      _RL resid, f, fp, hf, htmp
      _RL rhoi, rhow, r, i_r, delta
      INTEGER ITER, i, j, bi, bj
      _RL ETA_GL_STREAMICE
      EXTERNAL ETA_GL_STREAMICE
      _RL ETA_GL_STREAMICE_PRIME
!      EXTERNAL PHI_GL_STREAMICE_PRIME

      _RL H(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL Rtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL r_low_si_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /streamice_rlow_ad/ r_low_si_ad
      _RL h_streamice_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /streamice_fields_rl_ad/ h_streamice_ad

!       il=ILNBLNK( xx_genarr2d_file(iarr) )
!       write(fnamegeneric(1:80),'(2a,i10.10)')
!     &     xx_genarr2d_file(iarr)(1:il),'.',optimcycle
!       CALL ACTIVE_READ_XY ( fnamegeneric, tmpfld2d, 1,
!     &                      doglobalread, ladinit, optimcycle,
!     &                      myThid, xx_genarr2d_dummy(iarr) )



!      CALL ACTIVE_READ_XY ( 'H_adjust.data', H, 1,
!     &     doglobalread, ladinit, optimcycle,
!     &     myThid,

!      CALL READ_FLD_XY_RL( 'R_low_adjust.data', ' ', Rtmp,
!     &     0, myThid )

      CALL STREAMICE_INVERT_SURF_FORTHICK (
     O            H,
     I            surf_el_streamice,
     I            R_low_si,
     I            delta,
     I            myThid)

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1/r
      delta=1-r

      DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do j = 1,sNy
            do i = 1,sNx

              hf = -1.0 * i_r * R_low_si (i,j,bi,bj)

              fp = ETA_GL_STREAMICE_PRIME (
     &             H (i,j,bi,bj)-hf,
     &             delta,
     &             1. _d 0,
     &             delta*hf,
     &             streamice_smooth_gl_width)

              r_low_si_ad (i,j,bi,bj) =
     &          r_low_si_ad (i,j,bi,bj) -
     &          i_r * (fp-delta)/fp *
     &          h_streamice_ad(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo


#endif
      RETURN
      END
