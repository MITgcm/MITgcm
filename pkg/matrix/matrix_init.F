C $Header: /u/gcmpack/MITgcm/pkg/matrix/Attic/matrix_init.F,v 1.3 2007/11/05 18:58:00 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE MATRIX_INIT( myThid )

      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "MATRIX.h"
#include "PARAMS.h"

      INTEGER myThid

#ifdef ALLOW_MATRIX

C     local variables:
      INTEGER i, j, k, bi, bj
      INTEGER iTracer

C     Initialize counters
      expMatrixCounter=0
      impMatrixCounter=0
      DO iTracer=1,PTRACERS_num
        DO bj=myByLo(myThid), myByHi (myThid)
          DO bi=myBxLo (myThid), myBxHi (myThid)
            DO k=1,Nr
              DO j=1-Oly, sNy+Oly
                DO i=1-Olx, sNx+Olx
                    PTRACERS_initial(i,j,k,bi,bj,iTracer)=
     &                  pTracer(i,j,k,bi,bj,iTracer)
                  MATRIX(i,j,k,bi,bj,iTracer,1)= 0. _d 0
                  MATRIX(i,j,k,bi,bj,iTracer,2)= 0. _d 0
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

C      DO iTracer=1,PTRACERS_numInUse
C         DO bj=myByLo(myThid), myByHi (myThid)
C            DO bi=myBxLo (myThid), myBxHi (myThid)
C               DO k=1,Nr
C                  DO j=1-Oly, sNy+Oly
C                     DO i=1-Olx, sNx+Olx
C                        MATRIX(i,j,k,bi,bj,iTracer,1)= 0. _d 0
C                        MATRIX(i,j,k,bi,bj,iTracer,2)= 0. _d 0
C                     ENDDO
C                  ENDDO
C               ENDDO
C            ENDDO
C         ENDDO
C      ENDDO

#endif /* ALLOW_MATRIX */
      RETURN
      END
