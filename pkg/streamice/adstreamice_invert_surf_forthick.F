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
      _RL adr_low_si(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adstreamice_rlow/ adr_low_si 
      _RL adh_streamice(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adstreamice_fields_rl/ adh_streamice

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

              adr_low_si (i,j,bi,bj) = 
     &          adr_low_si (i,j,bi,bj) - 
     &          i_r * (fp-delta)/fp *
     &          adh_streamice(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

        
#endif
      RETURN
      END
