C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_pnh.F,v 1.5 2005/11/08 02:14:10 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PNH
C     !INTERFACE:
      SUBROUTINE INI_PNH( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PNH                                        
C     | o Set model initial non-hydrostatic potential.             
C     *==========================================================*
C     | There are several options for setting the initial         
C     | NH-potential (=Pressure/rhoConst) anomaly:
C     |  1. Inline code                                           
C     |  2. Three-dimensional data from a file.                     
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NH_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: My Thread Id number
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,I,J,K
CEOP

ceh3 needs an IF ( useNONHYDROSTATIC ) THEN

C--   Initialise surface position anomaly to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           phi_nh(I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Read an initial state
c     IF (phiNHinitFile .NE. ' ') THEN
c      _BEGIN_MASTER( myThid )
c      CALL READ_FLD_XY_RL( phiNHinitFile, ' ', phi_nh, 0, myThid )
c      _END_MASTER(myThid)
c     _EXCH_XYZ_R8(phi_nh, myThid)
c     ENDIF


#endif /* ALLOW_NONHYDROSTATIC */
      RETURN
      END
