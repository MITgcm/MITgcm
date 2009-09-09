C $Header: /u/gcmpack/MITgcm/model/src/taueddy_init_varia.F,v 1.4 2009/09/09 15:31:42 jmc Exp $
C $Name:  $

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
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_EDDYPSI

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     i,j,k
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           eddyPsiX(i,j,k,bi,bj)=0.
           eddyPsiY(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( eddyPsiXFile .NE. ' ' ) THEN
       CALL READ_FLD_XYZ_RS( eddyPsiXFile, ' ',
     &                       eddyPsiX, nIter0, myThid )
      ENDIF
      IF ( eddyPsiYFile .NE. ' ' ) THEN
       CALL READ_FLD_XYZ_RS( eddyPsiYFile, ' ',
     &                       eddyPsiY, nIter0, myThid )
      ENDIF

      CALL EXCH_UV_XYZ_RS(eddyPsiX,eddyPsiY,.TRUE.,myThid)

#endif /* ALLOW_EDDYPSI */

      RETURN
      END
