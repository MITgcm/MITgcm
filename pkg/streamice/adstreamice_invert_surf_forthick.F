#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif


      SUBROUTINE ADSTRMICE_H_INV (
#ifdef ALLOW_OPENAD
     &            H,
     &            Hd,
     &            S,
     &            R,
     &            Rd,
     &            delta,
#endif
     &            mythid)

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
#ifdef ALLOW_AUTODIFF

      _RL fp, hf
      _RL rhoi, rhow, i_r, delta
      INTEGER i, j, bi, bj
      _RL ETA_GL_STREAMICE
      EXTERNAL ETA_GL_STREAMICE
      _RL ETA_GL_PRIME_STREAMICE
!      EXTERNAL PHI_GL_STREAMICE_PRIME

      _RL H(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL Rtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)a
#ifdef ALLOW_AUTODIFF_TAMC
      _RL r_low_si_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /streamice_rlow_ad/ r_low_si_ad
      _RL h_streamice_ad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /streamice_fields_rl_ad/ h_streamice_ad
#else
      _RL R(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL S(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL Rd(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL Hd(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif

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

!      call write_fld_xy_rl ("R_invert_pre","",R,0,mythid)

      CALL STREAMICE_INVERT_SURF_FORTHICK (
     O            H,
#ifdef ALLOW_AUTODIFF_TAMC
     I            surf_el_streamice,
     I            R_low_si,
#else
     I            S,
     I            R,
#endif
     I            delta,
     I            myThid)

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      i_r = rhow/rhoi
      delta=1-rhoi/rhow

      DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do j = 1,sNy
            do i = 1,sNx


#ifdef ALLOW_AUTODIFF_TAMC
              hf = -1.0 * i_r * R_low_si (i,j,bi,bj)
#else
              hf = -1.0 * i_r * R (i,j,bi,bj)
#endif

              fp = ETA_GL_PRIME_STREAMICE (
     &             H (i,j,bi,bj)-hf,
     &             delta,
     &             1. _d 0,
     &             delta*hf,
     &             streamice_smooth_gl_width)


#ifdef ALLOW_AUTODIFF_TAMC
              r_low_si_ad (i,j,bi,bj) =
     &          r_low_si_ad (i,j,bi,bj) -
     &          i_r * (fp-delta)/fp *
     &          h_streamice_ad(i,j,bi,bj)
#else
              rd (i,j,bi,bj) =
     &          rd (i,j,bi,bj) -
     &          i_r * (fp-delta)/fp *
     &          hd(i,j,bi,bj)
#endif

            enddo
          enddo
        enddo
      enddo

#endif
#endif
      RETURN
      END
