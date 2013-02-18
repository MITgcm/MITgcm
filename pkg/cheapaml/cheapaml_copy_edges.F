C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_copy_edges.F,v 1.1 2013/02/18 21:17:11 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CBOP
C     !ROUTINE: CHEAPAML_COPY_EDGES
C     !INTERFACE:
      SUBROUTINE CHEAPAML_COPY_EDGES(
     I                     xIsPeriodic, yIsPeriodic,
     I                     refFld,
     U                     trcFld,
     I                     bi, bj, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CHEAPAML_COPY_EDGES
C     | o If not periodic, copy Edges of reference array "refFld"
C     |   into current tracer field array "trcFld"
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     xIsPeriodic :: periodic in X dir
C     yIsPeriodic :: periodic in Y dir
C     refFld      :: reference tracer field (copy from this field)
C     trcFld      :: current tracer field to update at the Edges
C     bi, bj      :: tile indices
C     myIter      :: current iteration number
C     myThid      :: my Thread Id number
      LOGICAL xIsPeriodic, yIsPeriodic
      _RL  refFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  trcFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj, myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i, j, iG, jG
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)

        IF (.NOT.xIsPeriodic) THEN
          iG = 1 + myXGlobalLo-1+(bi-1)*sNx
          IF (iG.EQ.1) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,1
               trcFld(i,j) = refFld(i,j)
             ENDDO
            ENDDO
          ENDIF
          iG = sNx + myXGlobalLo-1+(bi-1)*sNx
          IF (iG.GE.Nx) THEN
            DO j=1-OLy,sNy+OLy
             DO i=sNx,sNx+OLx
               trcFld(i,j) = refFld(i,j)
             ENDDO
            ENDDO
          ENDIF
        ENDIF

        IF (.NOT.yIsPeriodic) THEN
          jG = 1 + myYGlobalLo-1+(bj-1)*sNy
          IF (jG.EQ.1) THEN
            DO j=1-OLy,1
             DO i=1-OLx,sNx+OLx
               trcFld(i,j) = refFld(i,j)
             ENDDO
            ENDDO
          ENDIF
          jG = sNy + myYGlobalLo-1+(bj-1)*sNy
          IF (jG.GE.Ny) THEN
            DO j=sNy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               trcFld(i,j) = refFld(i,j)
             ENDDO
            ENDDO
          ENDIF
        ENDIF

c      ENDDO
c     ENDDO

      RETURN
      END
