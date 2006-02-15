C $Header: /u/gcmpack/MITgcm/model/src/taueddy_init_varia.F,v 1.1 2006/02/15 03:52:53 heimbach Exp $
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

#ifdef ALLOW_TAU_EDDY

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
           eddyTauX(i,j,k,bi,bj)=0.
           eddyTauY(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( eddyTauxFile .NE. ' ' ) THEN
       CALL MDSREADFIELD(eddyTauxFile,precFloat32,
     &                  'RS',Nr,eddyTauX,1,myThid)
      ENDIF
      IF ( eddyTauyFile .NE. ' ' ) THEN
       CALL MDSREADFIELD(eddyTauyFile,precFloat32,
     &                  'RS',Nr,eddyTauY,1,myThid)
      ENDIF

cph(
cph divide by rhoConst to suppress it in momentum eqn.
cph see taueddy_external_forcing
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           eddyTauX(i,j,k,bi,bj)=eddyTauX(i,j,k,bi,bj)
     &      *recip_rhoConst
           eddyTauY(i,j,k,bi,bj)=eddyTauY(i,j,k,bi,bj)
     &      *recip_rhoConst
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
cph)

      CALL EXCH_UV_XYZ_RS(eddyTauX,eddyTauY,.TRUE.,myThid)

#endif /* ALLOW_AUTODIFF_TAMC */

      END
