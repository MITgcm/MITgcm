C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_upd_ffrac_uncoupled.F,v 1.2 2014/09/05 14:28:00 dgoldberg Exp $
C $Name:  $


C this needs changes

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
      _RL OD, rhoi, rhow, delta, r, h, hf, i_r
#ifdef STREAMICE_SMOOTH_FLOATATION
      _RL ETA_GL_STREAMICE
      external ETA_GL_STREAMICE
      _RL PHI_GL_STREAMICE
      external PHI_GL_STREAMICE
#endif

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1/r
      delta=1-r


#ifdef STREAMICE_SMOOTH_FLOATATION

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
         if (STREAMICE_hmask(i,j,bi,bj).eq.1.0 .or.
     &        STREAMICE_hmask(i,j,bi,bj).eq.2.0) THEN

          
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
         ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#else

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
# ifdef USE_ALT_RLOW
          OD = -1.0 * R_low_si (i,j,bi,bj) - 
     &     H_streamice(i,j,bi,bj) * rhoi/rhow
# else
          OD = -1.0 * R_low (i,j,bi,bj) - 
     &     H_streamice(i,j,bi,bj) * rhoi/rhow
# endif


          IF (OD .ge. 0. _d 0) THEN          

c         ice thickness does not take up whole ocean column -> floating
           float_frac_streamice(i,j,bi,bj) = 0.0
# ifdef USE_ALT_RLOW
           base_el_streamice(i,j,bi,bj) = R_low_si(i,j,bi,bj)+OD
# else
           base_el_streamice(i,j,bi,bj) = R_low(i,j,bi,bj)+OD
# endif
           surf_el_streamice(i,j,bi,bj) = 
     &      (1-rhoi/rhow)*H_streamice(i,j,bi,bj)
          ELSE


           float_frac_streamice(i,j,bi,bj) = 1.0
# ifdef USE_ALT_RLOW
           base_el_streamice(i,j,bi,bj) = R_low_si(i,j,bi,bj)
           surf_el_streamice(i,j,bi,bj) = R_low_si(i,j,bi,bj)
# else
           base_el_streamice(i,j,bi,bj) = R_low(i,j,bi,bj)
           surf_el_streamice(i,j,bi,bj) = R_low(i,j,bi,bj) 
# endif
     &      + H_streamice(i,j,bi,bj)
          ENDIF
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
