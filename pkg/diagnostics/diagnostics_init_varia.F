C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_init_varia.F,v 1.6 2005/02/23 15:15:30 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INIT_VARIA(
     I     myThid )

C     !DESCRIPTION:
C     Initialize the qdiag array which accumulates during integration
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j,n,bi,bj
      CHARACTER*(80) fn

C     Zero out the qdiag array which accumulates during integration
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO n = 1,numdiags
            DO j = 1-Oly,sNy+Oly
              DO i = 1-Olx,sNx+Olx
                qdiag(i,j,n,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      CALL DIAGNOSTICS_READ_PICKUP( myThid )

      RETURN 
      END
