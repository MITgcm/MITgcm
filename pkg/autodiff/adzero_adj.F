
#include "CPP_OPTIONS.h"

C     !ROUTINE: ADZERO_ADJ_3D
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ_3D(
     I             bi, bj,myThid,adFieldFwd)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a 3D adjoint field
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL adFieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)

      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         adFieldFwd(i,j,k) = 0.
        ENDDO
       ENDDO
      ENDDO

      end

C     !ROUTINE: ADZERO_ADJ_2D
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ_2D(
     I             bi, bj,myThid,adFieldFwd)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a 2D adjoint field
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j
      _RL adFieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         adFieldFwd(i,j) = 0.
        ENDDO
       ENDDO

      end

