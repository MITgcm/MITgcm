C $Header: /u/gcmpack/MITgcm/pkg/autodiff/adzero_adj.F,v 1.7 2012/08/12 18:46:20 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--  File zero_adj.F:
C--   Contents
C--   o ADZERO_ADJ_1D
C--   o ADZERO_ADJ_LOC
C--   o ADZERO_ADJ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADZERO_ADJ_1D
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ_1D(
     I                  NrIn, adFieldFwd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a 1D adjoint field
C     | author: Gael Forget
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
      _RL adFieldFwd(NrIn)

C     !LOCAL VARIABLES:
      INTEGER k
CEOP

      DO k=1,NrIn
         adFieldFwd(k) = 0. _d 0
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADZERO_ADJ_LOC
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ_LOC(
     I                  NrIn, adFieldFwd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a local (no nsx,nsy dims.) adjoint field
C     | author: Gael Forget
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
      _RL adFieldFwd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn)

C     !LOCAL VARIABLES:
      INTEGER i,j,k
CEOP

      DO k=1,NrIn
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         adFieldFwd(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADZERO_ADJ
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ(
     I                  NrIn, adFieldFwd, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a full (incl. nsx,nsy dims.) adjoint field
C     | author: Gael Forget
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
      _RL adFieldFwd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrIn,nSx,nSy)

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,NrIn
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           adFieldFwd(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
