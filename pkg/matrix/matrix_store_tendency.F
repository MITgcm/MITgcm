C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_store_tendency.F,v 1.3 2007/11/05 18:58:00 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C     !ROUTINE MATRIX_STORE_TENDENCY_EXP
C     This routine accumalates the explicit tendency matrix. Note that
C     on entry gPtr is the tracer field after explicit advection-
C     diffusion. On exit, gPtr is set to the initial tracer field for
C     the next step (implicit matrix calculation).

      SUBROUTINE MATRIX_STORE_TENDENCY_EXP(
     &     bi, bj, myTime, myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "MATRIX.h"

      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MATRIX

      INTEGER i,j,k
      INTEGER iTracer

      DO iTracer=1,PTRACERS_numInUse
        DO k=1,Nr
          DO j=1-Oly, sNy+Oly
            DO i=1-Olx, sNx+Olx
              MATRIX(i,j,k,bi,bj,iTracer,1) =
     &             MATRIX(i,j,k,bi,bj,iTracer,1)
     &             + (gPtr(i,j,k,bi,bj,iTracer)
     &             - PTRACERS_initial(i,j,k,bi,bj,iTracer))
              gPtr(i,j,k,bi,bj,iTracer)=
     &             PTRACERS_initial(i,j,k,bi,bj,iTracer)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      expMatrixCounter = expMatrixCounter + 1

#endif /* ALLOW_MATRIX */
      RETURN
      END

C     !ROUTINE MATRIX_STORE_TENDENCY_IMP
C     This routine accumalates the implicit update matrix. Note that on
C     entry PTR is the tracer field after implicit advection-
C     diffusion. On exit, PTR is set to the initial tracer field.

      SUBROUTINE MATRIX_STORE_TENDENCY_IMP(
     &     bi, bj, myTime, myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "MATRIX.h"

      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MATRIX

      INTEGER i,j,k
      INTEGER iTracer

      DO iTracer=1,PTRACERS_numInUse
        DO k=1,Nr
          DO j=1-Oly, sNy+Oly
            DO i=1-Olx, sNx+Olx
              MATRIX(i,j,k,bi,bj,iTracer,2) =
     &             MATRIX(i,j,k,bi,bj,iTracer,2)
     &             +  pTracer(i,j,k,bi,bj,iTracer)
              pTracer(i,j,k,bi,bj,iTracer)=
     &             PTRACERS_initial(i,j,k,bi,bj,iTracer)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      impMatrixCounter = impMatrixCounter + 1

#endif /* ALLOW_MATRIX */
      RETURN
      END

