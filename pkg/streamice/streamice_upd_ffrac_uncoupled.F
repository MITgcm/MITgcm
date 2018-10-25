
#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
      SUBROUTINE STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )

C     !DESCRIPTION:
C     Initialize STREAMICE variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "GRID.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_STREAMICE

      INTEGER bi, bj, i, j
      _RL OD, rhoi, rhow, delta, r, h, hf, i_r, rlo
#ifdef STREAMICE_SMOOTH_FLOATATION2
      _RL ETA_GL_STREAMICE
      external ETA_GL_STREAMICE
#endif
#if defined(STREAMICE_SMOOTH_FLOATATION2) || defined(STREAMICE_SMOOTH_FLOATATION)
      _RL PHI_GL_STREAMICE
      external PHI_GL_STREAMICE
#endif
#ifdef STREAMICE_FIRN_CORRECTION
      _RL firn_depth
#endif

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1/r
      delta=1-r
#ifdef STREAMICE_FIRN_CORRECTION
      firn_depth = streamice_density * 
     &    streamice_firn_correction
     & / (streamice_density-streamice_density_firn)
#endif


#ifdef STREAMICE_SMOOTH_FLOATATION2

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          
          if (STREAMICE_hmask(i,j,bi,bj).eq.1.0 .or.
     &        STREAMICE_hmask(i,j,bi,bj).eq.2.0) THEN

           h = H_streamice(i,j,bi,bj)

# ifdef USE_ALT_RLOW
           hf = -1.0 * i_r * R_low_si (i,j,bi,bj) 
# else
           hf = -1.0 * i_r * R_low (i,j,bi,bj) 
# endif

           surf_el_streamice(i,j,bi,bj) =
     &      ETA_GL_STREAMICE (
     &       h-hf, 
     &       delta, 
     &       1. _d 0, 
     &       delta*hf,
     &       streamice_smooth_gl_width)

           base_el_streamice(i,j,bi,bj) =
     &      surf_el_streamice(i,j,bi,bj) - h

           float_frac_streamice(i,j,bi,bj) = 
     &      PHI_GL_STREAMICE (
     &       h-hf, 
     &       streamice_smooth_gl_width)

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#else 
! STREAMICE_SMOOTH_FLOATATION2

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          h = H_streamice(i,j,bi,bj)
# ifdef USE_ALT_RLOW
          rlo = R_low_si (i,j,bi,bj)
# else
          rlo = R_low (i,j,bi,bj)
#endif

#ifdef STREAMICE_FIRN_CORRECTION
          if (STREAMICE_apply_firn_correction) then
!           h=h_streamice(i,j,bi,bj)
          if (h_streamice(i,j,bi,bj).lt.firn_depth) then
            OD = -1.0 * rlo - streamice_density_firn/rhow *
     &       h_streamice(i,j,bi,bj)
          else
            OD = -1.0 * rlo - rhoi/rhow * 
     &       (h_streamice(i,j,bi,bj)-streamice_firn_correction)
          endif
          else
#endif
          OD = -1.0 * Rlo - 
     &     H * rhoi/rhow
#ifdef STREAMICE_FIRN_CORRECTION
          endif
#endif

          IF (OD .ge. 0. _d 0) THEN          

c         ice thickness does not take up whole ocean column -> floating
!           float_frac_streamice(i,j,bi,bj) = 0.0
           base_el_streamice(i,j,bi,bj) = Rlo+OD
#ifdef STREAMICE_FIRN_CORRECTION
           if (STREAMICE_apply_firn_correction) then
           if (h.lt.firn_depth) then
            surf_el_streamice(i,j,bi,bj) = 
     &      (1-streamice_density_firn/rhow)*h
           else
            surf_el_streamice(i,j,bi,bj) = 
     &      (1-rhoi/rhow)*h + 
     &        rhoi/rhow*streamice_firn_correction
           endif
           else
#endif
           surf_el_streamice(i,j,bi,bj) = 
     &      (1-rhoi/rhow)*H
#ifdef STREAMICE_FIRN_CORRECTION
           endif
#endif

          ELSE


!           float_frac_streamice(i,j,bi,bj) = 1.0
           base_el_streamice(i,j,bi,bj) = Rlo
           surf_el_streamice(i,j,bi,bj) = Rlo
     &      + H
          ENDIF

#ifndef STREAMICE_SMOOTH_FLOATATION


          IF (OD .ge. 0. _d 0) THEN          
           float_frac_streamice(i,j,bi,bj) = 0.0
          ELSE
           float_frac_streamice(i,j,bi,bj) = 1.0
          ENDIF

#else

          if (STREAMICE_hmask(i,j,bi,bj).eq.1.0 .or.
     &        STREAMICE_hmask(i,j,bi,bj).eq.2.0) THEN
# ifdef USE_ALT_RLOW
           hf = -1.0 * i_r * R_low_si (i,j,bi,bj) 
# else
           hf = -1.0 * i_r * R_low (i,j,bi,bj) 
# endif
           
           float_frac_streamice(i,j,bi,bj) = 
     &      PHI_GL_STREAMICE (
     &       h-hf+streamice_smooth_gl_width/2., 
     &       streamice_smooth_gl_width)

          endif
#endif

         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif

       _EXCH_XY_RL(float_frac_streamice, myThid )
       _EXCH_XY_RL(base_el_streamice, myThid )
       _EXCH_XY_RL(surf_el_streamice, myThid )

      


#endif /* ALLOW_STREAMICE */

      RETURN
      END
