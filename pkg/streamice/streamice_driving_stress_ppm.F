C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_driving_stress_ppm.F,v 1.1 2013/06/12 21:30:22 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_DRIVING_STRESS_PPM( myThid ) 
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


C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, k, l,
     &        Gi, Gj
      LOGICAL at_west_bdry, at_east_bdry, 
     &        at_north_bdry, at_south_bdry
      _RL sx, sy, diffx, diffy, neu_val
      
      IF (myXGlobalLo.eq.1) at_west_bdry = .true.
      IF (myYGlobalLo.eq.1) at_south_bdry = .true.
      IF (myXGlobalLo-1+sNx*nSx.eq.Nx) 
     & at_east_bdry = .false.
      IF (myYGlobalLo-1+sNy*nSy.eq.Ny) 
     & at_north_bdry = .false.

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          taudx_SI(i,j,bi,bj) = 0. _d 0
          taudy_SI(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        
        DO i=0,sNx+1
         DO j=0,sNy+1

          diffx = 0. _d 0
          diffy = 0. _d 0
          sx = 0. _d 0
          sy = 0. _d 0

          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j

          IF (STREAMICE_hmask(i,j,bi,bj).eq.1.0) THEN

           ! we are in an "active" cell

           IF (Gi.eq.1.AND..NOT.STREAMICE_EW_periodic) THEN

            ! western boundary - only one sided possible

            IF (STREAMICE_hmask(i+1,j,bi,bj).eq.1.0) THEN

             ! cell to east is active

             sx = (surf_el_streamice(i+1,j,bi,bj)-
     &             surf_el_streamice(i,j,bi,bj))/dxC(i+1,j,bi,bj)
            ELSE

             ! cell to east is empty

             sx = 0. _d 0
            ENDIF

            DO k=0,1
             DO l=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudx_SI(i+k,j+l,bi,bj) = taudx_SI(i+k,j+l,bi,bj) -
     &          0.25 * streamice_density * gravity * 
     &          (streamice_bg_surf_slope_x+sx) * 
     &          H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
              ENDIF
             ENDDO
            ENDDO

           ELSEIF (Gi.eq.Nx.AND..NOT.STREAMICE_EW_periodic) THEN

            ! eastern boundary - only one sided possible

            IF (STREAMICE_hmask(i-1,j,bi,bj).eq.1.0) THEN

             ! cell to west is active

             sx = (surf_el_streamice(i,j,bi,bj)-
     &             surf_el_streamice(i-1,j,bi,bj))/dxC(i,j,bi,bj)
            ELSE

             ! cell to west is inactive

             sx = 0. _d 0
            ENDIF

            DO k=0,1
             DO l=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudx_SI(i+k,j+l,bi,bj) = taudx_SI(i+k,j+l,bi,bj) -
     &          0.25 * streamice_density * gravity * 
     &          (streamice_bg_surf_slope_x+sx) * 
     &          H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
              ENDIF
             ENDDO
            ENDDO

           ELSE

            ! interior (west-east) cell

            IF (STREAMICE_hmask(i+1,j,bi,bj).eq.1.0 .and.
     &          STREAMICE_hmask(i-1,j,bi,bj).eq.1.0) THEN

             k = 0
             DO l=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudx_SI(i+k,j+l,bi,bj) = taudx_SI(i+k,j+l,bi,bj) -
     &          streamice_density * gravity * (1./6.) *
     &          (-2.*surf_el_streamice(i-1,j,bi,bj) + 
     &           surf_el_streamice(i,j,bi,bj) + 
     &           surf_el_streamice(i+1,j,bi,bj) +
     &           3.*streamice_bg_surf_slope_x * dxF(i,j,bi,bj)) *
     &          H_streamice(i,j,bi,bj) * .5 * dyF(i,j,bi,bj)
              ENDIF
             ENDDO

             k = 1
             DO l=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudx_SI(i+k,j+l,bi,bj) = taudx_SI(i+k,j+l,bi,bj) -
     &          streamice_density * gravity * (1./6.) *
     &          (-surf_el_streamice(i-1,j,bi,bj) - 
     &           surf_el_streamice(i,j,bi,bj) + 
     &           2*surf_el_streamice(i+1,j,bi,bj) +
     &           3.*streamice_bg_surf_slope_x * dxF(i,j,bi,bj)) *
     &          H_streamice(i,j,bi,bj) * .5 * dyF(i,j,bi,bj)
              ENDIF
             ENDDO


            ELSE 

             IF (STREAMICE_hmask(i+1,j,bi,bj).eq.1.0) THEN

              sx = (surf_el_streamice(i+1,j,bi,bj)-
     &             surf_el_streamice(i,j,bi,bj))/dxC(i+1,j,bi,bj)

             ELSEIF (STREAMICE_hmask(i-1,j,bi,bj).eq.1.0) THEN

              sx = (surf_el_streamice(i,j,bi,bj)-
     &             surf_el_streamice(i-1,j,bi,bj))/dxC(i,j,bi,bj)

             ELSE

              sx = 0. _d 0

             ENDIF

             DO k=0,1
              DO l=0,1
               IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
                taudx_SI(i+k,j+l,bi,bj) = taudx_SI(i+k,j+l,bi,bj) -
     &           0.25 * streamice_density * gravity * 
     &           (streamice_bg_surf_slope_x+sx) * 
     &           H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
               ENDIF
              ENDDO
             ENDDO

            ENDIF

           ENDIF

!!!!!!!! DONE WITH X-GRADIENT

           IF (Gj.eq.1.AND..NOT.STREAMICE_NS_periodic) THEN

            ! western boundary - only one sided possible

            IF (STREAMICE_hmask(i,j+1,bi,bj).eq.1.0) THEN

             ! cell to east is active

             sy = (surf_el_streamice(i,j+1,bi,bj)-
     &             surf_el_streamice(i,j,bi,bj))/dyC(i,j+1,bi,bj)
            ELSE

             ! cell to east is empty

             sy = 0. _d 0
            ENDIF

            DO k=0,1
             DO l=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudy_SI(i+k,j+l,bi,bj) = taudy_SI(i+k,j+l,bi,bj) -
     &          0.25 * streamice_density * gravity * 
     &          (streamice_bg_surf_slope_y+sy) * 
     &          H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
              ENDIF
             ENDDO
            ENDDO

           ELSEIF (Gj.eq.Ny.AND..NOT.STREAMICE_NS_periodic) THEN

            ! eastern boundary - only one sided possible

            IF (STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) THEN

             ! cell to west is active

             sy = (surf_el_streamice(i,j,bi,bj)-
     &             surf_el_streamice(i,j-1,bi,bj))/dyC(i,j,bi,bj)

            ELSE

             ! cell to west is inactive

             sy = 0. _d 0
            ENDIF

            DO k=0,1
             DO l=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudy_SI(i+k,j+l,bi,bj) = taudy_SI(i+k,j+l,bi,bj) -
     &          0.25 * streamice_density * gravity * 
     &          (streamice_bg_surf_slope_y+sy) * 
     &          H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
              ENDIF
             ENDDO
            ENDDO

           ELSE

            ! interior (west-east) cell

            IF (STREAMICE_hmask(i,j+1,bi,bj).eq.1.0 .and.
     &          STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) THEN

             l = 0
             DO k=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudy_SI(i+k,j+l,bi,bj) = taudy_SI(i+k,j+l,bi,bj) -
     &          streamice_density * gravity * (1./6.) *
     &          (-2.*surf_el_streamice(i,j-1,bi,bj) + 
     &           surf_el_streamice(i,j,bi,bj) + 
     &           surf_el_streamice(i,j+1,bi,bj) +
     &           3.*streamice_bg_surf_slope_y * dyF(i,j,bi,bj)) *
     &          H_streamice(i,j,bi,bj) * .5 * dxF(i,j,bi,bj)
              ENDIF
             ENDDO

             l = 1
             DO k=0,1
              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
               taudy_SI(i+k,j+l,bi,bj) = taudy_SI(i+k,j+l,bi,bj) -
     &          streamice_density * gravity * (1./6.) *
     &          (-surf_el_streamice(i,j-1,bi,bj) - 
     &           surf_el_streamice(i,j,bi,bj) + 
     &           2*surf_el_streamice(i,j+1,bi,bj) +
     &           3.*streamice_bg_surf_slope_y * dyF(i,j,bi,bj)) *
     &          H_streamice(i,j,bi,bj) * .5 * dxF(i,j,bi,bj)
              ENDIF
             ENDDO


            ELSE 

             IF (STREAMICE_hmask(i,j+1,bi,bj).eq.1.0) THEN

              sy = (surf_el_streamice(i,j+1,bi,bj)-
     &             surf_el_streamice(i,j,bi,bj))/dxC(i,j+1,bi,bj)

             ELSEIF (STREAMICE_hmask(i,j-1,bi,bj).eq.1.0) THEN

              sy = (surf_el_streamice(i,j,bi,bj)-
     &             surf_el_streamice(i,j-1,bi,bj))/dxC(i,j,bi,bj)

             ELSE

              sy = 0. _d 0

             ENDIF

             DO k=0,1
              DO l=0,1
               IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
                taudy_SI(i+k,j+l,bi,bj) = taudy_SI(i+k,j+l,bi,bj) -
     &           0.25 * streamice_density * gravity * 
     &           (streamice_bg_surf_slope_y+sy) * 
     &           H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
               ENDIF
              ENDDO
             ENDDO

            ENDIF

           ENDIF

!            DO k=0,1
!             DO l=0,1
!              IF (STREAMICE_umask(i+k,j+l,bi,bj).eq.1.0) THEN
!               taudx_SI(i+k,j+l,bi,bj) = taudx_SI(i+k,j+l,bi,bj) -
!      &         0.25 * streamice_density * gravity * 
!      &         (streamice_bg_surf_slope_x+sx) * 
!      &         H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
!               taudy_SI(i+k,j+l,bi,bj) = taudy_SI(i+k,j+l,bi,bj) -
!      &         0.25 * streamice_density * gravity * 
!      &         (streamice_bg_surf_slope_y+sy) * 
!      &         H_streamice(i,j,bi,bj) * rA(i,j,bi,bj)
!               
!              ENDIF
!             ENDDO
!            ENDDO

           IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) then
#ifdef USE_ALT_RLOW
            neu_val = .5 * gravity * 
     &       (streamice_density * H_streamice (i,j,bi,bj) ** 2 - 
     &        streamice_density_ocean_avg * R_low_si(i,j,bi,bj) ** 2)
#else
            neu_val = .5 * gravity * 
     &       (streamice_density * H_streamice (i,j,bi,bj) ** 2 - 
     &        streamice_density_ocean_avg * R_low(i,j,bi,bj) ** 2)
#endif
           ELSE
            neu_val = .5 * gravity * 
     &       (1-streamice_density/streamice_density_ocean_avg) * 
     &        streamice_density * H_streamice(i,j,bi,bj) ** 2 
           ENDIF

           IF ((STREAMICE_ufacemask(i,j,bi,bj) .eq. 2) 
     &      .OR. (STREAMICE_hmask(i-1,j,bi,bj) .eq. 0)
     &      .OR. (STREAMICE_hmask(i-1,j,bi,bj) .eq. 2) ) THEN ! left face of the cell is at a stress boundary
          ! the depth-integrated longitudinal stress is equal to the difference of depth-integrated pressure on either side of the face
          ! on the ice side, it is rho g h^2 / 2
          ! on the ocean side, it is rhow g (delta OD)^2 / 2
          ! OD can be zero under the ice; but it is ASSUMED on the ice-free side of the face, topography elevation is not above the base of the 
          !     ice in the current cell
             
             taudx_SI(i,j,bi,bj) = taudx_SI(i,j,bi,bj) - 
     &        .5 * dyG(i,j,bi,bj)*(neu_val+streamice_addl_backstress)
             taudx_SI(i,j+1,bi,bj) = taudx_SI(i,j+1,bi,bj) - 
     &        .5 * dyG(i,j,bi,bj)*(neu_val+streamice_addl_backstress)
           ENDIF

           IF ((STREAMICE_ufacemask(i+1,j,bi,bj) .eq. 2) 
     &      .OR. (STREAMICE_hmask(i+1,j,bi,bj) .eq. 0)
     &      .OR. (STREAMICE_hmask(i+1,j,bi,bj) .eq. 2) ) THEN 
             
             taudx_SI(i+1,j,bi,bj) = taudx_SI(i+1,j,bi,bj) + 
     &        .5 * dyG(i+1,j,bi,bj)*(neu_val+streamice_addl_backstress)  ! note negative sign is due to direction of normal vector
             taudx_SI(i+1,j+1,bi,bj) = taudx_SI(i+1,j+1,bi,bj) +
     &        .5 * dyG(i+1,j,bi,bj)*(neu_val+streamice_addl_backstress)
           ENDIF

           IF ((STREAMICE_vfacemask(i,j,bi,bj) .eq. 2) 
     &      .OR. (STREAMICE_hmask(i,j-1,bi,bj) .eq. 0)
     &      .OR. (STREAMICE_hmask(i,j-1,bi,bj) .eq. 2) ) THEN 
             
             taudy_SI(i,j,bi,bj) = taudy_SI(i,j,bi,bj) - 
     &        .5 * dxG(i,j,bi,bj)*(neu_val+streamice_addl_backstress)
             taudy_SI(i+1,j,bi,bj) = taudy_SI(i+1,j,bi,bj) - 
     &        .5 * dxG(i,j,bi,bj)*(neu_val+streamice_addl_backstress)
           ENDIF

           IF ((STREAMICE_vfacemask(i,j+1,bi,bj) .eq. 2) 
     &      .OR. (STREAMICE_hmask(i,j+1,bi,bj) .eq. 0)
     &      .OR. (STREAMICE_hmask(i,j+1,bi,bj) .eq. 2) ) THEN 
             
             taudy_SI(i,j+1,bi,bj) = taudy_SI(i,j+1,bi,bj) +
     &        .5 * dxG(i,j+1,bi,bj)*(neu_val+streamice_addl_backstress)
             taudy_SI(i+1,j+1,bi,bj) = taudy_SI(i+1,j+1,bi,bj) +
     &        .5 * dxG(i,j+1,bi,bj)*(neu_val+streamice_addl_backstress)
           ENDIF

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO



#endif
      RETURN
      END
       
      
