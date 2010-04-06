C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_init_fixed.F,v 1.5 2010/04/06 20:38:18 jmc Exp $
C $Name:  $

#include "RBCS_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_INIT_FIXED( myThid )

C !DESCRIPTION:
C calls subroutines that initialized fixed variables for relaxed
c boundary conditions

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif
#include "RBCS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_RBCS
C     !LOCAL VARIABLES:
C     i,j,k,bi,bj,iTracer  :: loop indices
      INTEGER i,j,k,bi,bj
      INTEGER irbc

C     Loop over mask index
      DO irbc=1,maskLEN

C     Loop over tiles
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

C        Initialize arrays in common blocks :
           DO k=1,Nr
            DO j=1-Oly,sNy+OLy
             DO i=1-Olx,sNx+Olx
               RBC_mask(i,j,k,bi,bj,irbc) = 0. _d 0
             ENDDO
            ENDDO
           ENDDO

C        end bi,bj loops
         ENDDO
        ENDDO
C     end of mask index loop
      ENDDO

C read in mask for relaxing
      DO irbc=1,maskLEN
       IF ( relaxMaskFile(irbc).NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RS(relaxMaskFile(irbc),' ',
     &                RBC_mask(1-Olx,1-Oly,1,1,1,irbc), 0, myThid)
         CALL EXCH_XYZ_RS( RBC_mask(1-Olx,1-Oly,1,1,1,irbc), myThid )
c        IF ( debugMode ) THEN
         IF ( debugLevel .GE. debLevB ) THEN
           _BEGIN_MASTER( myThid )
           CALL PLOT_FIELD_XYRS( RBC_mask(1-Olx,1-Oly,1,1,1,irbc),
     &                          'Boundary Relaxing' ,1, myThid )
           _END_MASTER(myThid)
        ENDIF
       ENDIF
      ENDDO

#endif /* ALLOW_RBCS */

      RETURN
      END
