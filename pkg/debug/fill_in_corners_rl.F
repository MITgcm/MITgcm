C $Header: /u/gcmpack/MITgcm/pkg/debug/fill_in_corners_rl.F,v 1.1 2003/12/07 20:36:16 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE FILL_IN_CORNERS_RL( 
     U              fld, 
     I              spvFld, Ksize,
     I              myThid )
C     *==========================================================*
C     | SUBROUTINE FILL_IN_CORNERS_RL
C     | o Fill-in the corner part of the halo region.
C     *==========================================================*
C     | Only used to test some configuration and scheme:
C     | put a special value in corners to test the impact
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fld   - input Field to modify 
C     spvFld - special value to fill-in the corner with
C     Ksize - 3rd dimension of local arrays (Input-Output field)
      INTEGER Ksize
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL spvFld
      INTEGER myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
      INTEGER ii, jj
      INTEGER bi, bj
 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1-Oly,0
          jj = j + sNy+Oly
          DO i=1-Olx,0
           ii = i + sNx+Olx
           fld(i,j,k,bi,bj)   = spvFld
           fld(ii,j,k,bi,bj)  = spvFld
           fld(i,jj,k,bi,bj)  = spvFld
           fld(ii,jj,k,bi,bj) = spvFld
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
 
      RETURN
      END
