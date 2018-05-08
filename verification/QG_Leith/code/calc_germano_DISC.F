#include "GMREDI.h"
#include "GMREDI_OPTIONS.h"

      SUBROUTINE CALC_GERMANO_DISC(
     I           field,bi,bj,k,epsilon,
     I           myThid )

      IMPLICIT NONE

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_TRACER_S2
C     | o Applies a discrete filter to "field" via one of the methods
C     | o in Sagaut and Grohens 1999
C     *==========================================================*
C     \ev

C     == Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#ifdef W2_FILL_NULL_REGIONS
#include "W2_EXCH2_PARAMS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments
C     field     :: cell-centered 2D field on which filter applies
C     tmpFld/2    :: working temporary arrays
C     a0, a1, a2   :: filter coefficients
C     epsilon    :: filter width


      INTEGER myThid
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpFld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL epsilon
      _RL a2,a1,a0

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,k,i,j,n
CEOP


C     Copy the field
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         tmpFld(i,j)=field(i,j)
        ENDDO
       ENDDO


C     Fourth order box filter
       a2 =  ((3.0 * epsilon**4.0) - (20.0 * epsilon**2.0))
     &   / 5760.0
       a1 =  ((-3.0 * epsilon**4.0) + (80.0 * epsilon**2.0))
     &   / 1440.0
       a0 = ((3.0 * epsilon**4.0) - (100.0 * epsilon**2.0)
     &    + 960.0) / 960.0 


C     Fourth order Gaussian filter
C       a2 =  ((1.0 * epsilon**4.0) - (4.0 * epsilon**2.0)) 
C     &   / 1152.0
C       a1 =  ((16.0 * epsilon**4.0) + (-1.0 * epsilon**2.0)) 
C     &   / 288.0
C       a0 = ((1.0 * epsilon**4.0) - (20.0 * epsilon**2.0) 
C     &    + 192.0) / 192.0


C     x-direction first
       DO j=1-Oly+2,sNy+Oly-2
        DO i=1-Olx+2,sNx+Olx-2
         tmpFld(i,j)=a2*field(i+2,j)+a2*field(i-2,j)
     &   +a1*field(i+1,j)+a1*field(i-1,j)+a0*field(i,j)
        ENDDO
       ENDDO     

C     now y-direction
       DO j=1-Oly+2,sNy+Oly-2
        DO i=1-Olx+2,sNx+Olx-2
         tmpFld2(i,j)=a2*tmpFld(i,j+2)+a2*tmpFld(i,j-2)
     &   +a1*tmpFld(i,j+1)+a1*tmpFld(i,j-1)+a0*tmpFld(i,j)
        ENDDO
       ENDDO


       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         field(i,j)=tmpFld2(i,j)
        ENDDO
       ENDDO



      RETURN
      END
