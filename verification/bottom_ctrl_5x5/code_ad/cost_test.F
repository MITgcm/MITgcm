#include "COST_OPTIONS.h"

      subroutine cost_test( myThid )
C     *==========================================================*
C     | subroutine cost_test
C     | o this routine computes the cost function for the tiles
C     |   of this processor
C     *==========================================================*
C     | Notes
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_TEST
C     == Local variables
      integer bi, bj
      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      _RL vol_trans
C--   index values at which the transport is to be calculated
      INTEGER iysecmin, iysecmax, ixsec
      PARAMETER (ixsec = 4, iysecmin = 3, iysecmax = 3)

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        vol_trans = 0.
        DO J=1,sNy
         jg = myYGlobalLo-1+(bj-1)*sNy+J
         IF ( jg .ge. iysecmin .and. jg .le. iysecmax ) THEN
          DO I=1,sNx
           ig = myXGlobalLo-1+(bi-1)*sNx+I
           IF ( ig .eq. ixsec ) THEN
            DO K=1,Nr
             IF ( maskW(I,J,K,BI,BJ) .NE. 0. ) THEN
              vol_trans = vol_trans
     &             + uVel(I,J,K,BI,BJ)
     &             *_hFacW(I,J,K,BI,BJ)
     &             *dyG(I,J,BI,BJ)*drF(K)
             ENDIF
            ENDDO
           ENDIF
          ENDDO
         ENDIF
        ENDDO
        objf_test(bi,bj) = vol_trans*1.0e-06
       END DO
      END DO
CML      objf_test(1,1) = vVel(3,3,1,1,1)* _hFacS(3,3,1,1,1)
Cml      iLocOut =  6
Cml      jLocOut =  35
Cml      kLocOut =  1
Cml
Cmlce    some reference temperature
Cml      thetaRef = 24.0 _d 0
Cml
CmlC--   Calculate cost function on tile of this instance
Cml      do bj = jtlo,jthi
Cml        do bi = itlo,ithi
Cml          do j=1,sNy
Cml          jg = myYGlobalLo-1+(bj-1)*sNy+j
Cml            do i=1,sNx
Cml            ig = myXGlobalLo-1+(bi-1)*sNx+i
Cml
Cml            if ((ig .eq. iLocOut) .and. (jg .eq. jLocOut)) then
Cml               write(*,'(a,3(x,i4),a,4(x,i4))')
Cml     &              'COST    ',ig,jg,kLocOut,' TILE ',i,j,bi,bj
Cml               objf_test(bi,bj) = theta(i,j,kLocOut,bi,bj)
Cml            endif
Cml
Cml            enddo
Cml          enddo
Cml        enddo
Cml      enddo

#endif /* ALLOW_COST_TEST */

      RETURN
      END
