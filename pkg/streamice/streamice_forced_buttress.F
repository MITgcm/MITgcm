#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_FORCED_BUTTRESS( myThid )
c      O taudx,
c      O taudy )

C     *============================================================*
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     *============================================================*
C     |                                                            |
C     *============================================================*
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
c       _RL taudy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k
      _RL unconf_stress, i_rhow
      _RL avg_density
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef STREAMICE_FIRN_CORRECTION
      _RL firn_depth, h
#endif

      i_rhow = 1./streamice_density_ocean_avg
#ifdef STREAMICE_FIRN_CORRECTION
      firn_depth = streamice_density *
     &    streamice_firn_correction
     & / (streamice_density-streamice_density_firn)
#endif /* STREAMICE_FIRN_CORRECTION */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
#ifdef STREAMICE_FIRN_CORRECTION
          if (STREAMICE_apply_firn_correction) then
           if (streamice_hmask(i,j,bi,bj).eq.1) then
            h = h_streamice(i,j,bi,bj)
            if (h.lt.firn_depth) then
             avg_density(i,j,bi,bj) = streamice_density_firn
            else
             avg_density(i,j,bi,bj) = streamice_density *
     &        (h - streamice_firn_correction) / h
            endif
           endif
          else
#endif /* STREAMICE_FIRN_CORRECTION */
           avg_density(i,j,bi,bj) = streamice_density
#ifdef STREAMICE_FIRN_CORRECTION
          endif
#endif /* STREAMICE_FIRN_CORRECTION */
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLy+1,sNx+OLy-1
c         taudx_SI(i,j,bi,bj) = 0. _d 0
c         taudy_SI(i,j,bi,bj) = 0. _d 0
          IF (streamice_hmask(i,j,bi,bj).eq.1.0) THEN

            ! baseline unconfined stress

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

              unconf_stress = gravity *
     &         (avg_density(i,j,bi,bj) * H_streamice(i,j,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &          streamice_density_ocean_avg * R_low_si(i,j,bi,bj)**2)
#else
     &          streamice_density_ocean_avg * R_low(i,j,bi,bj)**2)
#endif

            ELSE

#ifdef STREAMICE_FIRN_CORRECTION
             if (STREAMICE_apply_firn_correction) then

              if (H_streamice(i,j,bi,bj).lt.firn_depth) then
               unconf_stress =
     &          streamice_density_firn * gravity *
     &          (1-streamice_density_firn*i_rhow) *
     &          H_streamice(i,j,bi,bj)**2
              else
               unconf_stress = gravity * (
     &          streamice_density_firn * firn_depth**2 +
     &          (h_streamice(i,j,bi,bj)-firn_depth) *
     &           (streamice_density_firn*firn_depth+streamice_density*
     &           (h_streamice(i,j,bi,bj)-streamice_firn_correction)) -
     &          streamice_density**2*i_rhow*
     &           (h_streamice(i,j,bi,bj)-streamice_firn_correction)**2
     &          )
              endif

             else
#endif /* STREAMICE_FIRN_CORRECTION */

              unconf_stress = streamice_density * gravity *
     &         (1-streamice_density/streamice_density_ocean_avg) *
     &          H_streamice(i,j,bi,bj)**2

#ifdef STREAMICE_FIRN_CORRECTION
             endif
#endif /* STREAMICE_FIRN_CORRECTION */

            ENDIF

            ! right face

            if (streamice_ufacemask(i+1,j,bi,bj).eq.2.0) then

             do k=0,1
              if (streamice_umask(i+1,j+k,bi,bj).eq.1.0) then

c               if ((j+k).eq.10) then
c                print *, "GOT HERE 1", unconf_stress,
c     &               taudx_SI(i+1,j+k,bi,bj)
c               endif

               taudx_SI(i+1,j+k,bi,bj) = taudx_SI(i+1,j+k,bi,bj) +
     &          (streamice_u_normal_pert(i+1,j,bi,bj)  +
     &           streamice_u_normal_stress(i+1,j,bi,bj)) *
     &          .5 * unconf_stress * dyG(i+1,j,bi,bj)

               taudy_SI(i+1,j+k,bi,bj) = taudy_SI(i+1,j+k,bi,bj) +
     &         (streamice_v_shear_pert(i+1,j,bi,bj) +
     &          streamice_v_shear_stress(i+1,j,bi,bj)) *
     &          .5 * unconf_stress * dyG(i+1,j,bi,bj)

c               if ((j+k).eq.10) then
c                print *, "GOT HERE 1", taudx_SI(i+1,j+k,bi,bj)
c               endif

              endif
             enddo
            endif

            ! left face

            if (streamice_ufacemask(i,j,bi,bj).eq.2.0) then

             do k=0,1
              if (streamice_umask(i,j+k,bi,bj).eq.1.0) then

               taudx_SI(i,j+k,bi,bj) = taudx_SI(i,j+k,bi,bj) -
     &         (streamice_u_normal_pert(i,j,bi,bj) +
     &          streamice_u_normal_stress(i,j,bi,bj)) *
     &          .5 * unconf_stress * dyG(i,j,bi,bj)

               taudy_SI(i,j+k,bi,bj) = taudy_SI(i,j+k,bi,bj) -
     &         (streamice_v_shear_pert(i,j,bi,bj) +
     &          streamice_v_shear_stress(i,j,bi,bj)) *
     &          .5 * unconf_stress * dyG(i,j,bi,bj)

              endif
             enddo
            endif

            if (streamice_vfacemask(i,j+1,bi,bj).eq.2.0) then

             do k=0,1
              if (streamice_umask(i+k,j+1,bi,bj).eq.1.0) then

               taudy_SI(i+k,j+1,bi,bj) = taudy_SI(i+k,j+1,bi,bj) +
     &         (streamice_v_normal_pert(i,j+1,bi,bj) +
     &          streamice_v_normal_stress(i,j+1,bi,bj)) *
     &           .5 * dxG(i,j+1,bi,bj) * unconf_stress

               taudx_SI(i+k,j+1,bi,bj) = taudx_SI(i+k,j+1,bi,bj) +
     &         (streamice_u_shear_pert(i,j+1,bi,bj) +
     &          streamice_u_shear_stress(i,j+1,bi,bj)) *
     &          .5 * unconf_stress * dxG(i,j+1,bi,bj)

              endif
             enddo
            endif

            if (streamice_vfacemask(i,j,bi,bj).eq.2.0) then

             do k=0,1
              if (streamice_umask(i+k,j,bi,bj).eq.1.0) then

               taudy_SI(i+k,j,bi,bj) = taudy_SI(i+k,j,bi,bj) -
     &         (streamice_v_normal_pert(i,j,bi,bj) +
     &          streamice_v_normal_stress(i,j,bi,bj)) *
     &          .5 * dxG(i,j,bi,bj) * unconf_stress

               taudx_SI(i+k,j,bi,bj) = taudx_SI(i+k,j,bi,bj) -
     &         (streamice_u_shear_pert(i,j,bi,bj) +
     &          streamice_u_shear_stress(i,j,bi,bj)) *
     &          .5 * unconf_stress * dxG(i,j,bi,bj)

              endif
             enddo
            endif

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* STREAMICE_STRESS_BOUNDARY_CONTROL */
#endif /* ALLOW_STREAMICE */
      RETURN
      END
