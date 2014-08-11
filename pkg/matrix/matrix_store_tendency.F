C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_store_tendency.F,v 1.6 2014/08/11 20:29:06 jmc Exp $
C $Name:  $

#include "MATRIX_OPTIONS.h"

C--   File matrix_store_tendency.F:
C      Contents
C      o MATRIX_STORE_TENDENCY_EXP
C      o MATRIX_STORE_TENDENCY_IMP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MATRIX_STORE_TENDENCY_EXP

C !INTERFACE: ==========================================================
      SUBROUTINE MATRIX_STORE_TENDENCY_EXP(
     I                  iTracer, bi, bj,
     U                  tracNew,
     I                  myTime, myIter, myThid )

C !DESCRIPTION:
C     This routine accumalates the explicit tendency matrix. Note that
C     on entry gPtr is the tracer field after explicit advection-
C     diffusion. On exit, gPtr is set to the initial tracer field for
C     the next step (implicit matrix calculation).

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "MATRIX.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C     tracNew :: updated tracer field (after adding explicit contributions)
      INTEGER iTracer
      INTEGER bi, bj
      _RL     myTime
      _RL     tracNew(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MATRIX
C !LOCAL VARIABLES: ====================================================
      INTEGER i,j,k
CEOP

      DO k=1,Nr
       DO j=1-OLy, sNy+OLy
        DO i=1-OLx, sNx+OLx
          MATRIX(i,j,k,bi,bj,iTracer,1) = MATRIX(i,j,k,bi,bj,iTracer,1)
     &     + ( tracNew(i,j,k) - PTRACERS_initial(i,j,k,bi,bj,iTracer) )
          tracNew(i,j,k) = PTRACERS_initial(i,j,k,bi,bj,iTracer)
        ENDDO
       ENDDO
      ENDDO

C-    Increment counter when processing the last pTracer
      IF ( iTracer.EQ.PTRACERS_numInUse ) THEN
        expMatrixCounter(bi,bj) = expMatrixCounter(bi,bj) + 1
      ENDIF

#endif /* ALLOW_MATRIX */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MATRIX_STORE_TENDENCY_IMP

C !INTERFACE: ==========================================================
      SUBROUTINE MATRIX_STORE_TENDENCY_IMP(
     I                  bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C     This routine accumalates the implicit update matrix. Note that on
C     entry PTR is the tracer field after implicit advection-
C     diffusion. On exit, PTR is set to the initial tracer field.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "MATRIX.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MATRIX
C !LOCAL VARIABLES: ====================================================
      INTEGER iTracer
      INTEGER i,j,k
CEOP

      DO iTracer=1,PTRACERS_numInUse
        DO k=1,Nr
         DO j=1-OLy, sNy+OLy
          DO i=1-OLx, sNx+OLx
            MATRIX(i,j,k,bi,bj,iTracer,2) =
     &             MATRIX(i,j,k,bi,bj,iTracer,2)
     &             + pTracer(i,j,k,bi,bj,iTracer)
            pTracer(i,j,k,bi,bj,iTracer)=
     &             PTRACERS_initial(i,j,k,bi,bj,iTracer)
          ENDDO
         ENDDO
        ENDDO
      ENDDO
      impMatrixCounter(bi,bj) = impMatrixCounter(bi,bj) + 1

#endif /* ALLOW_MATRIX */
      RETURN
      END
