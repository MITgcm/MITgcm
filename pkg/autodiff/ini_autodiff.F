
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_AUTODIFF
C     !INTERFACE:
      SUBROUTINE INI_AUTODIFF( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_AUTODIFF                                    
C     | o Initialise to zero some active arrays  
C     |   These ini                
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef EXACT_CONSERV
# include "SURFACE.h"
#endif
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
# include "FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid

#ifdef ALLOW_AUTODIFF_TAMC

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
#ifdef EXACT_CONSERV
          hDivFlow(I,J,bi,bj)=0. _d 0
#endif
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
          EmPmR(I,J,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif /* ALLOW_AUTODIFF_TAMC */

      END
