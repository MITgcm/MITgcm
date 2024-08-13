#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_DRIVING_STRESS_FD( myThid )
c      O taudx,
c      O taudy )

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
c       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj
c     INTEGER Gi, Gj
      _RL dsdx
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dsdy
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL face_factor, grd_below_sl, i_rhow

      i_rhow = 1./streamice_density_ocean_avg

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          taudx_SI(i,j,bi,bj) = 0. _d 0
          taudy_SI(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

          IF (streamice_hmask(i,j,bi,bj) .eq. 1) THEN

c    ds/dx ------------------------------------------------

           IF (streamice_hmask(i-1,j,bi,bj) .eq. 1) THEN

            dsdx(i,j,bi,bj) = recip_dxC(i,j,bi,bj) *
     &       (surf_el_streamice(i,j,bi,bj) -
     &        surf_el_streamice(i-1,j,bi,bj))

           ELSEIF (streamice_hmask(i-1,j,bi,bj).eq.0.or.
     &             streamice_hmask(i-1,j,bi,bj).eq.2) THEN

            IF (streamice_hmask(i+1,j,bi,bj).eq.1) THEN
             dsdx(i,j,bi,bj) = recip_dxC(i+1,j,bi,bj) *
     &       (surf_el_streamice(i+1,j,bi,bj) -
     &        surf_el_streamice(i,j,bi,bj))
            ELSE
             dsdx(i,j,bi,bj) = 0.0
            ENDIF

           ELSE ! streamice_hmask(i-1,j,bi,bj).eq. -1
            dsdx(i,j,bi,bj) = 0.0
           ENDIF

c    ds/dy ------------------------------------------------

           IF (streamice_hmask(i,j-1,bi,bj) .eq. 1) THEN

            dsdy(i,j,bi,bj) = recip_dyC(i,j,bi,bj) *
     &       (surf_el_streamice(i,j,bi,bj) -
     &        surf_el_streamice(i,j-1,bi,bj))

           ELSEIF (streamice_hmask(i,j-1,bi,bj).eq.0.or.
     &             streamice_hmask(i,j-1,bi,bj).eq.2) THEN

            IF (streamice_hmask(i,j+1,bi,bj).eq.1) THEN
             dsdy(i,j,bi,bj) = recip_dyC(i,j+1,bi,bj) *
     &       (surf_el_streamice(i,j+1,bi,bj) -
     &        surf_el_streamice(i,j,bi,bj))
            ELSE
             dsdy(i,j,bi,bj) = 0.0
            ENDIF

           ELSE ! streamice_hmask(i-1,j,bi,bj).eq. -1
            dsdx(i,j,bi,bj) = 0.0
           ENDIF

c    end ------------------------------------------------

          ELSEIF(streamice_hmask(i,j,bi,bj).eq.0.or.
     &            streamice_hmask(i,j,bi,bj).eq.2) THEN

c    ds/dx ------------------------------------------------

           IF(streamice_hmask(i-1,j,bi,bj).eq.1.and.
     &         streamice_hmask(i-2,j,bi,bj).eq.1) THEN

             dsdx(i,j,bi,bj) = recip_dxC(i-1,j,bi,bj) *
     &       (surf_el_streamice(i-1,j,bi,bj) -
     &        surf_el_streamice(i-2,j,bi,bj))

           ELSE
            dsdx(i,j,bi,bj) = 0.0

           ENDIF

c    ds/dy ------------------------------------------------

           IF(streamice_hmask(i,j-1,bi,bj).eq.1.and.
     &         streamice_hmask(i,j-2,bi,bj).eq.1) THEN

             dsdy(i,j,bi,bj) = recip_dyC(i,j-1,bi,bj) *
     &       (surf_el_streamice(i,j-1,bi,bj) -
     &        surf_el_streamice(i,j-2,bi,bj))

           ELSE
            dsdy(i,j,bi,bj) = 0.0

           ENDIF

c    end ------------------------------------------------

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL(dsdy, myThid)
      _EXCH_XY_RL(dsdx, myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO i=1,sNx
         DO j=1,sNy

c           Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
c           Gj = (myYGlobalLo-1)+(bj-1)*sNy+j

          IF (streamice_hmask(i,j,bi,bj).eq.1.0) THEN

           IF (streamice_umask(i,j,bi,bj).eq.1.0) THEN

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i,j,bi,bj) *
     &       (0.5 * dyG(i,j,bi,bj) + 0.25 * dyF(i,j,bi,bj)) *
     &       0.5 * dxG(i,j,bi,bj)

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i+1,j,bi,bj) *
     &       0.25 * dyF(i,j,bi,bj) *
     &       0.5 * dxG(i,j,bi,bj)

           ENDIF

           IF (streamice_umask(i+1,j,bi,bj).eq.1.0) THEN

            taudx_si(i+1,j,bi,bj) = taudx_si(i+1,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i+1,j,bi,bj) *
     &       (0.5 * dyG(i+1,j,bi,bj) + 0.25 * dyF(i,j,bi,bj)) *
     &       0.5 * dxG(i,j,bi,bj)

            taudx_si(i+1,j,bi,bj) = taudx_si(i+1,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i,j,bi,bj) *
     &       0.25 * dyF(i,j,bi,bj) *
     &       0.5 * dxG(i,j,bi,bj)

           ENDIF

           IF (streamice_umask(i,j+1,bi,bj).eq.1.0) THEN

            taudx_si(i,j+1,bi,bj) = taudx_si(i,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i,j,bi,bj) *
     &       (0.5 * dyG(i,j,bi,bj) + 0.25 * dyF(i,j,bi,bj)) *
     &       0.5 * dxG(i,j+1,bi,bj)

            taudx_si(i,j+1,bi,bj) = taudx_si(i,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i+1,j,bi,bj) *
     &       0.25 * dyF(i,j,bi,bj) *
     &       0.5 * dxG(i,j+1,bi,bj)

           ENDIF

           IF (streamice_umask(i+1,j+1,bi,bj).eq.1.0) THEN

            taudx_si(i+1,j+1,bi,bj) = taudx_si(i+1,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i+1,j,bi,bj) *
     &       (0.5 * dyG(i+1,j,bi,bj) + 0.25 * dyF(i,j,bi,bj)) *
     &       0.5 * dxG(i,j+1,bi,bj)

            taudx_si(i+1,j+1,bi,bj) = taudx_si(i+1,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdx (i,j,bi,bj) *
     &       0.25 * dyF(i,j,bi,bj) *
     &       0.5 * dxG(i,j+1,bi,bj)

           ENDIF

#ifdef USE_ALT_RLOW
           IF (R_low_si(i,j,bi,bj) .lt. 0. _d 0) then
#else
           IF (R_low(i,j,bi,bj) .lt. 0. _d 0) then
#endif
              grd_below_sl = 1. _d 0
           else
              grd_below_sl = 0. _d 0
           endif

c check face to right
           IF (streamice_hmask(i+1,j,bi,bj).eq.0.or.
     &         streamice_hmask(i+1,j,bi,bj).eq.2.or.
     &         streamice_ufacemask(i+1,j,bi,bj).eq.2) THEN

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

             face_factor =
     &        0.25 * dyG(i+1,j,bi,bj) *
     &        gravity *
     &        streamice_density * H_streamice(i,j,bi,bj)**2-
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low_si(i,j,bi,bj)**2
#else
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low(i,j,bi,bj)**2
#endif

            ELSE

             face_factor =
     &        0.25 * dyG(i+1,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density*i_rhow) *
     &         H_streamice(i,j,bi,bj)**2

            ENDIF

            taudx_si(i+1,j,bi,bj) = taudx_si(i+1,j,bi,bj)
     &       - face_factor

            taudx_si(i+1,j+1,bi,bj) = taudx_si(i+1,j+1,bi,bj)
     &       - face_factor

           ENDIF

c check face to left
           IF (streamice_hmask(i-1,j,bi,bj).eq.0.or.
     &         streamice_hmask(i-1,j,bi,bj).eq.2.or.
     &         streamice_ufacemask(i,j,bi,bj).eq.2) THEN

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

             face_factor =
     &        0.25 * dyG(i,j,bi,bj) *
     &        gravity *
     &        streamice_density * H_streamice(i,j,bi,bj)**2-
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low_si(i,j,bi,bj)**2
#else
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low(i,j,bi,bj)**2
#endif

            ELSE

             face_factor =
     &        0.25 * dyG(i,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density*i_rhow) *
     &         H_streamice(i,j,bi,bj)**2

            ENDIF

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj)
     &       + face_factor

            taudx_si(i,j+1,bi,bj) = taudx_si(i,j+1,bi,bj)
     &       + face_factor

           ENDIF

c Y FORCES

           IF (streamice_vmask(i,j,bi,bj).eq.1.0) THEN

            taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j,bi,bj) *
     &       (0.5 * dxG(i,j,bi,bj) + 0.25 * dxF(i,j,bi,bj)) *
     &       0.5 * dyG(i,j,bi,bj)

            taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j+1,bi,bj) *
     &       0.25 * dxF(i,j,bi,bj) *
     &       0.5 * dyG(i,j,bi,bj)

           ENDIF

           IF (streamice_vmask(i,j+1,bi,bj).eq.1.0) THEN

            taudy_si(i,j+1,bi,bj) = taudy_si(i,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j+1,bi,bj) *
     &       (0.5 * dxG(i,j+1,bi,bj) + 0.25 * dxF(i,j,bi,bj)) *
     &       0.5 * dyG(i,j,bi,bj)

            taudy_si(i,j+1,bi,bj) = taudy_si(i,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j,bi,bj) *
     &       0.25 * dxF(i,j,bi,bj) *
     &       0.5 * dyG(i,j,bi,bj)

           ENDIF

           IF (streamice_vmask(i+1,j,bi,bj).eq.1.0) THEN

            taudy_si(i+1,j,bi,bj) = taudy_si(i+1,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j,bi,bj) *
     &       (0.5 * dxG(i,j,bi,bj) + 0.25 * dxF(i,j,bi,bj)) *
     &       0.5 * dyG(i+1,j,bi,bj)

            taudy_si(i+1,j,bi,bj) = taudy_si(i+1,j,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j+1,bi,bj) *
     &       0.25 * dxF(i,j,bi,bj) *
     &       0.5 * dyG(i+1,j,bi,bj)

           ENDIF

           IF (streamice_umask(i+1,j+1,bi,bj).eq.1.0) THEN

            taudy_si(i+1,j+1,bi,bj) = taudy_si(i+1,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j+1,bi,bj) *
     &       (0.5 * dxG(i,j+1,bi,bj) + 0.25 * dxF(i,j,bi,bj)) *
     &       0.5 * dyG(i,j+1,bi,bj)

            taudy_si(i+1,j+1,bi,bj) = taudy_si(i+1,j+1,bi,bj) +
     &       gravity * streamice_density *
     &       H_streamice (i,j,bi,bj) * dsdy (i,j,bi,bj) *
     &       0.25 * dxF(i,j,bi,bj) *
     &       0.5 * dyG(i,j+1,bi,bj)

           ENDIF

#ifdef USE_ALT_RLOW
           IF (R_low_si(i,j,bi,bj) .lt. 0. _d 0) then
#else
           IF (R_low(i,j,bi,bj) .lt. 0. _d 0) then
#endif
              grd_below_sl = 1. _d 0
           else
              grd_below_sl = 0. _d 0
           endif

c check face to right
           IF (streamice_hmask(i,j+1,bi,bj).eq.0.or.
     &         streamice_hmask(i,j+1,bi,bj).eq.2.or.
     &         streamice_vfacemask(i,j+1,bi,bj).eq.2) THEN

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

             face_factor =
     &        0.25 * dxG(i,j+1,bi,bj) *
     &        gravity *
     &        streamice_density * H_streamice(i,j,bi,bj)**2-
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low_si(i,j,bi,bj)**2
#else
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low(i,j,bi,bj)**2
#endif

            ELSE

             face_factor =
     &        0.25 * dxG(i,j+1,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density*i_rhow) *
     &         H_streamice(i,j,bi,bj)**2

            ENDIF

            taudy_si(i,j+1,bi,bj) = taudy_si(i,j+1,bi,bj)
     &       - face_factor

            taudy_si(i+1,j+1,bi,bj) = taudy_si(i+1,j+1,bi,bj)
     &       - face_factor

           ENDIF

c check face to left
           IF (streamice_hmask(i,j-1,bi,bj).eq.0.or.
     &         streamice_hmask(i,j-1,bi,bj).eq.2.or.
     &         streamice_vfacemask(i,j,bi,bj).eq.2) THEN

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

             face_factor =
     &        0.25 * dxG(i,j,bi,bj) *
     &        gravity *
     &        streamice_density * H_streamice(i,j,bi,bj)**2-
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low_si(i,j,bi,bj)**2
#else
     &         streamice_density_ocean_avg * grd_below_sl *
     &         R_low(i,j,bi,bj)**2
#endif

            ELSE

             face_factor =
     &        0.25 * dxG(i,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density*i_rhow) *
     &         H_streamice(i,j,bi,bj)**2

            ENDIF

            taudy_si(i,j,bi,bj) = taudy_SI(i,j,bi,bj)
     &       + face_factor

            taudy_si(i+1,j,bi,bj) = taudy_si(i+1,j,bi,bj)
     &       + face_factor

           ENDIF

          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          taudx_SI(i,j,bi,bj) = -1.*taudx_SI(i,j,bi,bj)
          taudy_SI(i,j,bi,bj) = -1.*taudy_SI(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
c       call write_fld_xy_rl ('driving_taux','',taudx_si,0,myThid)

#endif
      RETURN
      END
