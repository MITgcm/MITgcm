C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_forced_buttress.F,v 1.2 2014/06/04 12:56:40 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_FORCED_BUTTRESS( myThid ) 
!      O taudx, 
!      O taudy )

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
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l
      LOGICAL at_west_bdry, at_east_bdry, 
     &        at_north_bdry, at_south_bdry
      _RL unconf_stress
      

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLy+1,sNx+OLy-1
!          taudx_SI(i,j,bi,bj) = 0. _d 0
!          taudy_SI(i,j,bi,bj) = 0. _d 0
           if (streamice_hmask(i,j,bi,bj).eq.1.0) then

            ! baseline unconfined stress

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

              unconf_stress = gravity *
     &         (streamice_density * H_streamice(i,j,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &          streamice_density_ocean_avg * R_low_si(i,j,bi,bj)**2)
#else           
     &          streamice_density_ocean_avg * R_low(i,j,bi,bj)**2)
#endif

            ELSE

              unconf_stress = streamice_density * gravity *
     &         (1-streamice_density/streamice_density_ocean_avg) *
     &          H_streamice(i,j,bi,bj)**2

            ENDIF

            ! right face

            if (streamice_ufacemask(i+1,j,bi,bj).eq.2.0) then

             do k=0,1
              if (streamice_umask(i+1,j+k,bi,bj).eq.1.0) then


               taudx_SI(i+1,j+k,bi,bj) = taudx_SI(i+1,j+k,bi,bj) +
     &          (streamice_u_normal_pert(i+1,j,bi,bj)  + 
     &           streamice_u_normal_stress(i+1,j,bi,bj)) *
     &          .5 * unconf_stress * dyG(i+1,j,bi,bj)

               taudy_SI(i+1,j+k,bi,bj) = taudy_SI(i+1,j+k,bi,bj) +
     &         (streamice_v_shear_pert(i+1,j,bi,bj) +
     &          streamice_v_shear_stress(i+1,j,bi,bj)) *
     &          .5 * unconf_stress * dyG(i+1,j,bi,bj)

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
          END IF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif
#endif
      RETURN
      END
       
      
