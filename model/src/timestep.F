C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.18 2000/09/11 20:53:25 heimbach Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R TIMESTEP                                             |
C     | o Step model fields forward in time                      |
C     \==========================================================/
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax,
     I                     K,
     I                     myIter, myThid )
      implicit none

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      INTEGER myIter, myThid

C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05

C     Adams-Bashforth timestepping weights
Caja  IF (myIter .EQ. 0) THEN
Caja   ab15=1.0
Caja   ab05=0.0
Caja  ELSE
       ab15=1.5+abeps
       ab05=-0.5-abeps
Caja  ENDIF

C     Step forward zonal velocity (store in Gu)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gUNm1(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gU(i,j,k,bi,bj)+ab05*gUNm1(i,j,k,bi,bj)
#ifdef INCLUDE_CD_CODE
     &    +guCD(i,j,k,bi,bj)
#endif
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
       ENDDO

C     Step forward meridional velocity (store in Gv)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gVNm1(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)
     &    +deltaTmom*(ab15*gV(i,j,k,bi,bj)+ab05*gVNm1(i,j,k,bi,bj)
#ifdef INCLUDE_CD_CODE
     &    +gvCD(i,j,k,bi,bj)
#endif
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO

C     Step forward temperature
      IF (tempStepping) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gTNm1(i,j,k,bi,bj)=theta(i,j,k,bi,bj)
     &    +deltaTtracer*(ab15*gT(i,j,k,bi,bj)+ab05*gTNm1(i,j,k,bi,bj))
        ENDDO
       ENDDO
      ENDIF

C     Step forward salt
      IF (saltStepping) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gSNm1(i,j,k,bi,bj)=salt(i,j,k,bi,bj)
     &    +deltaTtracer*(ab15*gS(i,j,k,bi,bj)+ab05*gSNm1(i,j,k,bi,bj))
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
