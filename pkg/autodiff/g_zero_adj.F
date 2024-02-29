#include "CPP_EEOPTIONS.h"

C--  File zero_adj.F:
C--   Contents
C--   o ZERO_ADJ_1D
C--   o ZERO_ADJ_LOC
C--   o ZERO_ADJ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ZERO_ADJ_1D
C     !INTERFACE:
      SUBROUTINE G_ZERO_ADJ_1D(
     I                NrIn, fieldFwd, fieldFwd_tl, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a one dimensional adjoint field
C     | ... nothing happens in forward mode
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL fieldFwd(NrIn)
      _RL fieldFwd_tl(NrIn)
CEOP

C     !LOCAL VARIABLES:
      INTEGER k
CEOP

      DO k=1,NrIn
         FieldFwd_tl(k) = 0. _d 0
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ZERO_ADJ_LOC
C     !INTERFACE:
      SUBROUTINE G_ZERO_ADJ_LOC(
     I                NrIn, fieldFwd, fieldFwd_tl, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a local (no nsx,nsy dims.) tlm field
C     | ... nothing happens in forward mode
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL fieldFwd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn)
      _RL fieldFwd_tl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn)

C     !LOCAL VARIABLES:
      INTEGER i,j,k
CEOP

      DO k=1,NrIn
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         fieldFwd_tl(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ZERO_ADJ
C     !INTERFACE:
      SUBROUTINE G_ZERO_ADJ(
     I                NrIn, fieldFwd, fieldFwd_tl, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a full (incl. nsx,nsy dims.) TLM field
C     | ... nothing happens in forward mode
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER NrIn, myThid
      _RL fieldFwd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)
      _RL fieldFwd_tl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,NrIn
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fieldFwd_tl(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
