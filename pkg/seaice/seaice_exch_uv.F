C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_exch_uv.F,v 1.2 2004/12/27 20:34:11 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE SEAICE_EXCH_UV ( ufld, vfld, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_EXCH_UV                                |
C     | o RL exchange routine with k=3                           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      _RL ufld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL vfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      INTEGER myThid
CEndofinterface

C     == Local variables ==
      INTEGER i,j,k,bi,bj
      _RL tempU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tempV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   Update overlap regions 
      DO k=1,3
         DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
               DO j=1-Oly,sNy+OLy
                  DO i=1-OLx,sNx+OLx
                     tempU(i,j,bi,bj)=ufld(i,j,k,bi,bj)
                     tempV(i,j,bi,bj)=vfld(i,j,k,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
         CALL EXCH_UV_XY_RL(tempU,tempV,.TRUE.,myThid)
         DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
               DO j=1-Oly,sNy+OLy
                  DO i=1-OLx,sNx+OLx
                     ufld(i,j,k,bi,bj)=tempU(i,j,bi,bj)
                     vfld(i,j,k,bi,bj)=tempV(i,j,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
