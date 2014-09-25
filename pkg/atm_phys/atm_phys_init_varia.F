C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_init_varia.F,v 1.2 2014/09/25 19:44:33 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_INIT_VARIA

C !INTERFACE:
      SUBROUTINE ATM_PHYS_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize Atm_Phys variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_ATM_PHYS

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Initialize arrays in common blocks :
C     3-dim.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           atmPhys_dT(i,j,k,bi,bj) = 0. _d 0
           atmPhys_dQ(i,j,k,bi,bj) = 0. _d 0
           atmPhys_dU(i,j,k,bi,bj) = 0. _d 0
           atmPhys_dV(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C     2-dim.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          atmPhys_SST(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C- end bi,bj loops
       ENDDO
      ENDDO

C--   Initialize Boundary conditions:
      IF ( atmPhys_SSTFile .NE. ' ' ) THEN
         CALL READ_FLD_XY_RL( atmPhys_SSTFile,' ',
     &                        atmPhys_SST, 0, myThid )
         CALL EXCH_XY_RL( atmPhys_SST, myThid )
      ENDIF

C--   Initialize ATM_PHYS State Variables:
      IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &                           .AND. pickupSuff.EQ.' ' ) THEN
C-    start new run from iter-zero:

      ELSE
C-    restart from a pickup:
         CALL ATM_PHYS_READ_PICKUP( nIter0, myThid )

C-    end start-from-iter-zero if/else block
      ENDIF

#endif /* ALLOW_ATM_PHYS */

      RETURN
      END
