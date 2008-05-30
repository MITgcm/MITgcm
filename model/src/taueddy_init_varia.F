C $Header: /u/gcmpack/MITgcm/model/src/taueddy_init_varia.F,v 1.2 2008/05/30 02:45:43 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TAUEDDY_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE TAUEDDY_INIT_VARIA( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TAUEDDY_INIT_VARIA                                    
C     | o Initialise to zero some active arrays  
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid

#ifdef ALLOW_EDDYPSI

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I, J, K
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           eddyPsiX(i,j,k,bi,bj)=0.
           eddyPsiY(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( eddyPsixFile .NE. ' ' ) THEN
       CALL MDSREADFIELD(eddyPsiXFile,precFloat32,
     &                  'RS',Nr,eddyPsiX,1,myThid)
      ENDIF
      IF ( eddyPsiyFile .NE. ' ' ) THEN
       CALL MDSREADFIELD(eddyPsiYFile,precFloat32,
     &                  'RS',Nr,eddyPsiY,1,myThid)
      ENDIF

      CALL EXCH_UV_XYZ_RS(eddyPsiX,eddyPsiY,.TRUE.,myThid)

#endif /* ALLOW_AUTODIFF_TAMC */

      END
