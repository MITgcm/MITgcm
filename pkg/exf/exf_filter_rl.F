C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_filter_rl.F,v 1.10 2012/01/03 17:49:46 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_FILTER_RL(
     U                          arr,
     I                          ckind, myThid )

C     ==================================================================
C     SUBROUTINE EXF_FILTER_RL
C     ==================================================================
C
C     o apply mask to input field
C
C     ==================================================================
C     SUBROUTINE EXF_FILTER_RL
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "EXF_CONSTANTS.h"

C     == routine arguments ==
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*1 ckind
      INTEGER myThid

C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j, ks

C     == end of interface ==

C     filter forcing field array
      IF ( ckind.NE.' ' ) THEN

        ks = 1
        IF ( usingPCoords ) ks = Nr

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

C         Set undefined values to zero.
Crg not necessary and
Crg would require additional intermediate results in adjoint
crg          DO j = 1,sNy
crg            DO i = 1,sNx
crg              IF (arr(i,j,bi,bj) .LE. exf_undef) THEN
crg                arr(i,j,bi,bj) = 0. _d 0
crg              ENDIF
crg            ENDDO
crg          ENDDO

C         Set land points to zero
          IF     ( ckind .EQ. 'c' ) THEN
            DO j = 1,sNy
             DO i = 1,sNx
               IF ( maskC(i,j,ks,bi,bj) .EQ. 0. ) THEN
                 arr(i,j,bi,bj) = 0. _d 0
               ENDIF
             ENDDO
            ENDDO
          ELSEIF ( ckind .EQ. 'w' ) THEN
            DO j = 1,sNy
             DO i = 1,sNx
               IF ( maskW(i,j,ks,bi,bj) .EQ. 0. ) THEN
                 arr(i,j,bi,bj) = 0. _d 0
               ENDIF
             ENDDO
            ENDDO
          ELSEIF ( ckind .EQ. 's' ) THEN
            DO j = 1,sNy
             DO i = 1,sNx
               IF ( maskS(i,j,ks,bi,bj) .EQ. 0. ) THEN
                 arr(i,j,bi,bj) = 0. _d 0
               ENDIF
             ENDDO
            ENDDO
          ENDIF

         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
