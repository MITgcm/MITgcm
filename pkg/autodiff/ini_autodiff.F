C $Header: /u/gcmpack/MITgcm/pkg/autodiff/Attic/ini_autodiff.F,v 1.9 2007/08/16 04:15:18 heimbach Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

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
#include "FFIELDS.h"
#include "SURFACE.h"

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
      INTEGER  I, J, K
CEOP

C--   Scalar fields
      TsurfCor=0. _d 0
      SsurfCor=0. _d 0

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
#ifdef ALLOW_EP_FLUX
           EfluxY(i,j,k,bi,bj)=0.
           EfluxP(i,j,k,bi,bj)=0.
#endif
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
#ifdef EXACT_CONSERV
         dEtaHdt(i,j,bi,bj) = 0. _d 0
         PmEpR(i,j,bi,bj) = 0. _d 0
cph          hDivFlow(I,J,bi,bj)=0. _d 0
#endif
#ifdef ALLOW_EXF
          EmPmR(I,J,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif /* ALLOW_AUTODIFF_TAMC */

      END
