#include "LAYERS_OPTIONS.h"

CBOP
      SUBROUTINE LAYERS_LOCATE(
     I                          xx,n,m,sNx,sNy,x,
     O                          k,
     I                          myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | Find the index(-array) k such that x is between xx(k)
C     | and xx(k+1) by bisection, following Press et al.,
C     | Numerical Recipes in Fortran. xx must be monotonic.
C     *==========================================================*
C     \ev

C !USES:
      IMPLICIT NONE
C !INPUT PARAMETERS:
C     xx        :: array of bin-boundaries (layers_boundaries)
C     n         :: length of xx
C     m         :: int(log2(n)) + 1 = length of bisection loop
C     sNx,sNy   :: size of index array and input x
C     x         :: input array of values
C     k         :: index array (output)
C     myThid    :: my Thread Id number
      INTEGER n,m,sNx,sNy
      _RL     xx(1:n+1)
      _RL     x(sNx+1,sNy+1)
      INTEGER k(sNx+1,sNy+1)
      INTEGER myThid

C !LOCAL VARIABLES:
C     i,j      :: horizontal indices
C     l        :: bisection loop index
C     kl,ku,km :: work arrays and variables
      INTEGER i,j
CEOP
#ifdef TARGET_NEC_SX
      INTEGER l, km
      INTEGER kl(sNx+1,sNy+1), ku(sNx+1,sNy+1)

C     bisection, following Press et al., Numerical Recipes in Fortran,
C     mostly, because it can be vectorized
      DO j = 1,sNy+1
       DO i = 1,sNx+1
        kl(i,j)=1
        ku(i,j)=n+1
       END DO
      END DO
      DO l = 1,m
       DO j = 1,sNy+1
        DO i = 1,sNx+1
         IF (ku(i,j)-kl(i,j).GT.1) THEN
          km=(ku(i,j)+kl(i,j))/2
CML       IF ((xx(n).GE.xx(1)).EQV.(x(i,j).GE.xx(km))) THEN
          IF ( ((xx(n).GE.xx(1)).AND.(x(i,j).GE.xx(km))).OR.
     &         ((xx(n).GE.xx(1)).AND.(x(i,j).GE.xx(km))) ) THEN
           kl(i,j)=km
          ELSE
           ku(i,j)=km
          END IF
         END IF
        END DO
       END DO
      END DO
      DO j = 1,sNy+1
       DO i = 1,sNx+1
        IF ( x(i,j).LT.xx(2) ) THEN
         k(i,j)=1
        ELSE IF ( x(i,j).GE.xx(n) ) THEN
         k(i,j)=n
        ELSE
         k(i,j)=kl(i,j)
        END IF
       END DO
      END DO
#else
C     the old way
      DO j = 1,sNy+1
       DO i = 1,sNx+1
        IF (x(i,j) .GE. xx(n)) THEN
C     the point is in the hottest bin or hotter
         k(i,j) = n
        ELSE IF (x(i,j) .LT. xx(2)) THEN
C        the point is in the coldest bin or colder
         k(i,j) = 1
        ELSE IF ( (x(i,j) .GE. xx(k(i,j)))
     &    .AND.   (x(i,j) .LT. xx(k(i,j)+1)) ) THEN
C     already on the right bin -- do nothing
        ELSE IF (x(i,j) .GE. xx(k(i,j))) THEN
C     have to hunt for the right bin by getting hotter
         DO WHILE (x(i,j) .GE. xx(k(i,j)+1))
          k(i,j) = k(i,j) + 1
         ENDDO
C     now xx(k) < x <= xx(k+1)
        ELSE IF (x(i,j) .LT. xx(k(i,j)+1)) THEN
C     have to hunt for the right bin by getting colder
         DO WHILE (x(i,j) .LT. xx(k(i,j)))
          k(i,j) = k(i,j) - 1
         ENDDO
C     now xx(k) <= x < xx(k+1)
        ELSE
C     that should have covered all the options
         k(i,j) = -1
        ENDIF

       ENDDO
      ENDDO
#endif /* TARGET_NEC_SX */

#endif /* ALLOW_LAYERS */

      RETURN
      END
