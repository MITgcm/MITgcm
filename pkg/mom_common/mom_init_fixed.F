C$Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_init_fixed.F,v 1.2 2005/09/29 03:33:28 jmc Exp $
C$Name:  $

#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE MOM_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize fixed quantities
C      for momentum (common to fluxform & vecinv) packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     i,j,k,bi,bj  :: loop counter
      INTEGER i,j,k,bi,bj
#endif

#ifdef NONLIN_FRSURF
C-    used for side-drag:
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            h0FacC(i,j,k,bi,bj) = hFacC(i,j,k,bi,bj)
            h0FacW(i,j,k,bi,bj) = hFacW(i,j,k,bi,bj)
            h0FacS(i,j,k,bi,bj) = hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
       ENDDO
      ENDDO
#endif /* NONLIN_FRSURF */


#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MOM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
