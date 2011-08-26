C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_init_varia.F,v 1.2 2011/08/26 19:47:00 jmc Exp $
C $Name:  $

#include "MATRIX_OPTIONS.h"

      SUBROUTINE MATRIX_INIT_VARIA( myThid )

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
      expMatrixWriteCount = 1
      impMatrixWriteCount = 1
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         expMatrixCounter(bi,bj) = 0
         impMatrixCounter(bi,bj) = 0
       ENDDO
      ENDDO
      DO iTracer=1,PTRACERS_num
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           DO k=1,Nr
            DO j=1-Oly, sNy+Oly
             DO i=1-Olx, sNx+Olx
               PTRACERS_initial(i,j,k,bi,bj,iTracer) =
     &                  pTracer(i,j,k,bi,bj,iTracer)
               MATRIX(i,j,k,bi,bj,iTracer,1) = 0. _d 0
               MATRIX(i,j,k,bi,bj,iTracer,2) = 0. _d 0
             ENDDO
            ENDDO
           ENDDO
         ENDDO
        ENDDO
      ENDDO

#endif /* ALLOW_MATRIX */
      RETURN
      END
