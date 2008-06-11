C $Header: /u/gcmpack/MITgcm/pkg/autodiff/adzero_adj.F,v 1.3 2008/06/11 18:29:11 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     !ROUTINE: ADZERO_ADJ_LOC
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ_LOC(
     I             NrIn,myThid,adFieldFwd)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a local (no nsx,nsy dims.) adjoint field
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL adFieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn)

      DO k=1,NrIn
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         adFieldFwd(i,j,k) = 0.
        ENDDO
       ENDDO
      ENDDO

      end


C     !ROUTINE: ADZERO_ADJ
C     !INTERFACE:
      SUBROUTINE ADZERO_ADJ(
     I             NrIn,myThid,adFieldFwd)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o zero out a full (incl. nsx,nsy dims.) adjoint field
C     | author: Gael Forget
C     *==========================================================*

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid,bi,bj,i,j,k
      _RL adFieldFwd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,NrIn,nSx,nSy)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,NrIn
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           adFieldFwd(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      end

