C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_driving_stress.F,v 1.6 2015/01/19 18:32:28 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_DRIVING_STRESS( myThid ) 
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
      _RL grd_below_sl
      _RL sx, sy, diffx, diffy, geom_fac
      
      IF (myXGlobalLo.eq.1) at_west_bdry = .true.
      IF (myYGlobalLo.eq.1) at_south_bdry = .true.
      IF (myXGlobalLo-1+sNx*nSx.eq.Nx) 
     & at_east_bdry = .false.
      IF (myYGlobalLo-1+sNy*nSy.eq.Ny) 
     & at_north_bdry = .false.

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
        
        DO i=1,sNx
         DO j=1,sNy

          diffx = 0. _d 0
          diffy = 0. _d 0
          sx = 0. _d 0
          sy = 0. _d 0

          Gi = (myXGlobalLo-1)+(bi-1)*sNx+i
          Gj = (myYGlobalLo-1)+(bj-1)*sNy+j

          IF (streamice_umask(i,j,bi,bj).eq.1.0) THEN

           IF (streamice_hmask(i-1,j,bi,bj).eq.1.0.AND.
     &      streamice_hmask(i,j,bi,bj).eq.1.0) THEN
         
!             geom_fac = sqrt(rA(i-1,j,bi,bj)*recip_rA(i,j,bi,bj)*
!      &        dxF(i,j,bi,bj)*recip_dxF(i-1,j,bi,bj))

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) - 
     &       0.25 * dyG(i,j,bi,bj) *
     &       streamice_density * gravity *
     &       (H_streamice(i,j,bi,bj)+
     &        H_streamice(i-1,j,bi,bj)) *
     &       (surf_el_streamice(i,j,bi,bj)-
     &              surf_el_streamice(i-1,j,bi,bj))

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) -
     &       streamice_density * gravity * 
     &       streamice_bg_surf_slope_x * .25 * rA(i,j,bi,bj) *
     &       (H_streamice(i-1,j,bi,bj) + H_streamice(i,j,bi,bj))

           ELSE IF (streamice_hmask(i-1,j,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i-1,j,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i-1,j,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i-1,j,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif
     
             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) +
     &        0.25 * dyG(i,j,bi,bj) *
     &        gravity * 
     &        (streamice_density * H_streamice(i-1,j,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low_si(i-1,j,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i-1,j,bi,bj)**2)
#endif

            ELSE

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) +
     &        0.25 * dyG(i,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) *
     &         H_streamice(i-1,j,bi,bj)**2 

            ENDIF

           ELSE IF (streamice_hmask(i,j,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i,j,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i,j,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0 
             else
              grd_below_sl = 0. _d 0
             endif

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) -
     &        0.25 * dyG(i,j,bi,bj) *
     &        gravity *
     &        (streamice_density * H_streamice(i,j,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low_si(i,j,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i,j,bi,bj)**2)
#endif

            ELSE

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) -
     &        0.25 * dyG(i,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) *
     &         H_streamice(i,j,bi,bj)**2
 
            END IF
           END IF

! cells below
 
           IF (streamice_hmask(i-1,j-1,bi,bj).eq.1.0.AND.
     &      streamice_hmask(i,j-1,bi,bj).eq.1.0) THEN
         
!             geom_fac = sqrt(rA(i-1,j-1,bi,bj)*recip_rA(i,j-1,bi,bj)*
!      &             dxF(i,j-1,bi,bj)*recip_dxF(i-1,j-1,bi,bj))

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) - 
     &       0.25 * dyg(i,j-1,bi,bj) *
     &       streamice_density * gravity *
     &       (H_streamice(i,j-1,bi,bj)+
     &        H_streamice(i-1,j-1,bi,bj)) *
     &       (surf_el_streamice(i,j-1,bi,bj)-
     &              surf_el_streamice(i-1,j-1,bi,bj))

            taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) -
     &       streamice_density * gravity *
     &       streamice_bg_surf_slope_x * .25 * rA(i,j-1,bi,bj) * 
     &       (H_streamice(i-1,j-1,bi,bj) + H_streamice(i,j-1,bi,bj))

           ELSE IF (streamice_hmask(i-1,j-1,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i-1,j-1,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i-1,j-1,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i-1,j-1,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) +
     &        0.25 * dyg(i,j-1,bi,bj) *
     &        gravity * 
     &        (streamice_density * H_streamice(i-1,j-1,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg*grd_below_sl * 
     &         R_low_si(i-1,j-1,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i-1,j-1,bi,bj)**2)
#endif

            ELSE

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) +
     &        0.25 * dyg(i,j-1,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) * 
     &         H_streamice(i-1,j-1,bi,bj)**2 

            ENDIF

           ELSE IF (streamice_hmask(i,j-1,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i,j-1,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i,j-1,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i,j-1,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) -
     &        0.25 * dyg(i,j-1,bi,bj) *
     &        gravity *
     &        (streamice_density * H_streamice(i,j-1,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low_si(i,j-1,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i,j-1,bi,bj)**2)
#endif

            ELSE

             taudx_si(i,j,bi,bj) = taudx_si(i,j,bi,bj) -
     &        0.25 * dyg(i,j-1,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) *
     &         H_streamice(i,j-1,bi,bj)**2

            END IF 
           END IF
          END IF       ! if umask==1 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (streamice_vmask(i,j,bi,bj).eq.1.0) THEN

           IF (streamice_hmask(i,j-1,bi,bj).eq.1.0.AND.
     &      streamice_hmask(i,j,bi,bj).eq.1.0) THEN
         
!             geom_fac = sqrt(rA(i,j-1,bi,bj)*recip_rA(i,j,bi,bj)*
!      &        dxF(i,j,bi,bj)*recip_dyF(i,j-1,bi,bj))

            taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) - 
     &       0.25 * dxG(i,j,bi,bj) *
     &       streamice_density * gravity *
     &       (H_streamice(i,j,bi,bj)+
     &        H_streamice(i,j-1,bi,bj)) *
     &       (surf_el_streamice(i,j,bi,bj)-
     &              surf_el_streamice(i,j-1,bi,bj))

            taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &       streamice_density * gravity *
     &       streamice_bg_surf_slope_y * .25 * rA(i,j,bi,bj) * 
     &       (H_streamice(i,j-1,bi,bj) + H_streamice(i,j,bi,bj))

           ELSE IF (streamice_hmask(i,j-1,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i,j-1,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i,j-1,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i,j-1,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) +
     &        0.25 * dxG(i,j,bi,bj) *
     &        gravity * 
     &        (streamice_density * H_streamice(i,j-1,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low_si(i,j-1,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i,j-1,bi,bj)**2)
#endif

            ELSE

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) +
     &        0.25 * dxG(i,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) * 
     &         H_streamice(i,j-1,bi,bj)**2 

            ENDIF

           ELSE IF (streamice_hmask(i,j,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i,j,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i,j,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i,j,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &        0.25 * dxG(i,j,bi,bj) *
     &        gravity *
     &        (streamice_density * H_streamice(i,j,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low_si(i,j,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i,j,bi,bj)**2)
#endif

            ELSE

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &        0.25 * dxG(i,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) *
     &         H_streamice(i,j,bi,bj)**2
 
            END IF
           END IF

! cells to left
 
           IF (streamice_hmask(i-1,j-1,bi,bj).eq.1.0.AND.
     &      streamice_hmask(i-1,j,bi,bj).eq.1.0) THEN
         
!             geom_fac = sqrt(rA(i-1,j-1,bi,bj)*recip_rA(i-1,j,bi,bj)*
!      &        dxF(i-1,j,bi,bj)*recip_dxF(i-1,j-1,bi,bj))

            taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &       0.25 * dxG(i-1,j,bi,bj) *
     &       streamice_density * gravity *
     &       (H_streamice(i-1,j,bi,bj)+
     &        H_streamice(i-1,j-1,bi,bj)) *
     &       (surf_el_streamice(i-1,j,bi,bj)-
     &              surf_el_streamice(i-1,j-1,bi,bj))

            taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &       streamice_density * gravity *
     &       streamice_bg_surf_slope_y * .25 * rA(i-1,j,bi,bj) *
     &       (H_streamice(i-1,j-1,bi,bj) + H_streamice(i-1,j,bi,bj))


           ELSE IF (streamice_hmask(i-1,j-1,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i-1,j-1,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i-1,j-1,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i-1,j-1,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) +
     &        0.25 * dxG(i-1,j,bi,bj) *
     &        gravity * 
     &        (streamice_density * H_streamice(i-1,j-1,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg*grd_below_sl * 
     &         R_low_si(i-1,j-1,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i-1,j-1,bi,bj)**2)
#endif

            ELSE

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) +
     &        0.25 * dxG(i-1,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) * 
     &         H_streamice(i-1,j-1,bi,bj)**2 

            ENDIF

           ELSE IF (streamice_hmask(i-1,j,bi,bj).eq.1.0) THEN

            IF (float_frac_streamice(i-1,j,bi,bj) .eq. 1.0) THEN

#ifdef USE_ALT_RLOW
             IF (R_low_si(i-1,j,bi,bj) .lt. 0. _d 0) then
#else           
             IF (R_low(i-1,j,bi,bj) .lt. 0. _d 0) then
#endif             
              grd_below_sl = 1. _d 0
             else
              grd_below_sl = 0. _d 0
             endif

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &        0.25 * dxG(i-1,j,bi,bj) *
     &        gravity *
     &        (streamice_density * H_streamice(i-1,j,bi,bj)**2 -
#ifdef USE_ALT_RLOW
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low_si(i-1,j,bi,bj)**2)
#else           
     &         streamice_density_ocean_avg * grd_below_sl * 
     &         R_low(i-1,j,bi,bj)**2)
#endif

            ELSE

             taudy_si(i,j,bi,bj) = taudy_si(i,j,bi,bj) -
     &        0.25 * dxG(i-1,j,bi,bj) *
     &        streamice_density * gravity *
     &        (1-streamice_density/streamice_density_ocean_avg) *
     &         H_streamice(i-1,j,bi,bj)**2

            END IF 
           END IF
          END IF      ! if vmask ==1

         ENDDO
        ENDDO
       ENDDO
      ENDDO

!      taudx_SI (1,1,1,1) = taudx_SI (1,1,1,1) +
!     & streamice_v_normal_pert (1,1,1,1)
      call write_fld_xy_rl("TAUDX_SI","",taudx_si,0,mythid)
      call write_fld_xy_rl("TAUDY_SI","",taudy_si,0,mythid)



#endif
      RETURN
      END
       
      
