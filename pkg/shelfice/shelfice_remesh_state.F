#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_REMESH_STATE
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_STATE(
     I                    k1SurfC, k1SurfW, k1SurfS, mrgFacC,
     U                    mrgFacW, mrgFacS,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_STATE
C     | o Update state variables near the surface
C     |   where remeshing occurs
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     k1SurfC   :: surface level index (at cell center) before remeshing
C     k1SurfW   :: surface level index (at cell W.Edge) before remeshing
C     k1SurfS   :: surface level index (at cell S.Edge) before remeshing
C     mrgFacC   :: merging weight for tracers (cell center)
C     mrgFacW   :: merging weight for U component velocity
C     mrgFacS   :: merging weight for V component velocity
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      INTEGER k1SurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k1SurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k1SurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mrgFacC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL mrgFacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL mrgFacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE_REMESHING
C     !LOCAL VARIABLES:
C     bi,bj   :: tile indices
C     i,j,k   :: Loop counters
C     k1, k2  :: previous and new surface level
C     ioUnit  :: temp for writing msg unit
C     msgBuf  :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k1, k2
      _RL r_newDz

C--  Update model state: 1) tracers
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          k1 = k1SurfC(i,j,bi,bj)
          k2 = kSurfC(i,j,bi,bj)
          IF ( k2.LT.k1 .AND. k1.LE.Nr ) THEN
C-    Splitting top cell (=k1):
C     just copy tracer value from former top (=k1) into new top cell (=k2)
            theta(i,j,k2,bi,bj) = theta(i,j,k1,bi,bj)
            salt(i,j,k2,bi,bj)  = salt(i,j,k1,bi,bj)
#ifdef ALLOW_ADAMSBASHFORTH_3
            gtNm(i,j,k2,bi,bj,1) = gtNm(i,j,k1,bi,bj,1)
            gtNm(i,j,k2,bi,bj,2) = gtNm(i,j,k1,bi,bj,2)
            gsNm(i,j,k2,bi,bj,1) = gsNm(i,j,k1,bi,bj,1)
            gsNm(i,j,k2,bi,bj,2) = gsNm(i,j,k1,bi,bj,2)
#else
            gtNm1(i,j,k2,bi,bj) = gtNm1(i,j,k1,bi,bj)
            gsNm1(i,j,k2,bi,bj) = gsNm1(i,j,k1,bi,bj)
#endif
          ENDIF
          IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-    Merging top cell (=k1) into the one just below (=k2):
C     average tracer into new top cell (=k2) using weight factor "mrgFacC"
            theta(i,j,k2,bi,bj) =
     &            theta(i,j,k1,bi,bj)*mrgFacC(i,j,1,bi,bj)
     &          + theta(i,j,k2,bi,bj)*mrgFacC(i,j,2,bi,bj)
            theta(i,j,k1,bi,bj) = 0.0
            salt (i,j,k2,bi,bj) =
     &            salt(i,j,k1,bi,bj)*mrgFacC(i,j,1,bi,bj)
     &          + salt(i,j,k2,bi,bj)*mrgFacC(i,j,2,bi,bj)
            salt(i,j,k1,bi,bj) = 0.0
#ifdef ALLOW_ADAMSBASHFORTH_3
            gtNm(i,j,k2,bi,bj,1) =
     &            gtNm(i,j,k1,bi,bj,1)*mrgFacC(i,j,1,bi,bj)
     &          + gtNm(i,j,k2,bi,bj,1)*mrgFacC(i,j,2,bi,bj)
            gtNm(i,j,k2,bi,bj,2) =
     &            gtNm(i,j,k1,bi,bj,2)*mrgFacC(i,j,1,bi,bj)
     &          + gtNm(i,j,k2,bi,bj,2)*mrgFacC(i,j,2,bi,bj)
            gtNm(i,j,k1,bi,bj,1)= 0.0
            gtNm(i,j,k1,bi,bj,2)= 0.0
C-
            gsNm(i,j,k2,bi,bj,1) =
     &            gsNm(i,j,k1,bi,bj,1)*mrgFacC(i,j,1,bi,bj)
     &          + gsNm(i,j,k2,bi,bj,1)*mrgFacC(i,j,2,bi,bj)
            gsNm(i,j,k2,bi,bj,2) =
     &            gsNm(i,j,k1,bi,bj,2)*mrgFacC(i,j,1,bi,bj)
     &          + gsNm(i,j,k2,bi,bj,2)*mrgFacC(i,j,2,bi,bj)
            gsNm(i,j,k1,bi,bj,1)= 0.0
            gsNm(i,j,k1,bi,bj,2)= 0.0
#else
            gtNm1(i,j,k2,bi,bj) =
     &            gtNm1(i,j,k1,bi,bj)*mrgFacC(i,j,1,bi,bj)
     &          + gtNm1(i,j,k2,bi,bj)*mrgFacC(i,j,2,bi,bj)
            gtNm1(i,j,k1,bi,bj) = 0.0
            gsNm1(i,j,k2,bi,bj) =
     &            gsNm1(i,j,k1,bi,bj)*mrgFacC(i,j,1,bi,bj)
     &          + gsNm1(i,j,k2,bi,bj)*mrgFacC(i,j,2,bi,bj)
            gsNm1(i,j,k1,bi,bj) = 0.0
#endif
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--  Update merging weights for both components of horizontal velocity
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          IF ( kSurfW(i,j,bi,bj).NE.k1SurfW(i,j,bi,bj) ) THEN
            k1 = k1SurfW(i,j,bi,bj)
            k2 =  kSurfW(i,j,bi,bj)
            IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-      merging former (=k1) into new (=k2) surface grid cell
c             r_newDz = recip_drF(k2)/hFacW(i,j,k2,bi,bj)
              r_newDz = recip_drF(k2)*recip_hFacW(i,j,k2,bi,bj)
              mrgFacW(i,j,1,bi,bj) = mrgFacW(i,j,1,bi,bj)
     &                              *drF(k1)*r_newDz
              mrgFacW(i,j,2,bi,bj) = mrgFacW(i,j,2,bi,bj)
     &                              *drF(k2)*r_newDz
            ENDIF
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          IF ( kSurfS(i,j,bi,bj).NE.k1SurfS(i,j,bi,bj) ) THEN
            k1 = k1SurfS(i,j,bi,bj)
            k2 =  kSurfS(i,j,bi,bj)
            IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-      merging former (=k1) into new (=k2) surface grid cell
c             r_newDz = recip_drF(k2)/hFacS(i,j,k2,bi,bj)
              r_newDz = recip_drF(k2)*recip_hFacS(i,j,k2,bi,bj)
              mrgFacS(i,j,1,bi,bj) = mrgFacS(i,j,1,bi,bj)
     &                              *drF(k1)*r_newDz
              mrgFacS(i,j,2,bi,bj) = mrgFacS(i,j,2,bi,bj)
     &                              *drF(k2)*r_newDz
            ENDIF
          ENDIF
         ENDDO
        ENDDO

C--  Update model state: 2) horizontal velocity components U & V
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          IF ( k1SurfW(i,j,bi,bj).NE.kSurfW(i,j,bi,bj) ) THEN
           k1 = k1SurfW(i,j,bi,bj)
           k2 =  kSurfW(i,j,bi,bj)
           IF ( k2.LT.k1 .AND. k1.LE.Nr ) THEN
C-      spliting case: just copy former surface value (=k1) into new one (=k2)
            uVel(i,j,k2,bi,bj) = uVel(i,j,k1,bi,bj)
#ifdef ALLOW_ADAMSBASHFORTH_3
            guNm(i,j,k2,bi,bj,1) = guNm(i,j,k1,bi,bj,1)
            guNm(i,j,k2,bi,bj,2) = guNm(i,j,k1,bi,bj,2)
#else
            guNm1(i,j,k2,bi,bj) = guNm1(i,j,k1,bi,bj)
#endif
           ENDIF
           IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-      merging former (=k1) into new (=k2) surface grid cell
            uVel(i,j,k2,bi,bj) =
     &            uVel(i,j,k1,bi,bj)*mrgFacW(i,j,1,bi,bj)
     &          + uVel(i,j,k2,bi,bj)*mrgFacW(i,j,2,bi,bj)
            uVel(i,j,k1,bi,bj) = 0.0
#ifdef ALLOW_ADAMSBASHFORTH_3
            guNm(i,j,k2,bi,bj,1) =
     &            guNm(i,j,k1,bi,bj,1)*mrgFacW(i,j,1,bi,bj)
     &          + guNm(i,j,k2,bi,bj,1)*mrgFacW(i,j,2,bi,bj)
            guNm(i,j,k2,bi,bj,2) =
     &            guNm(i,j,k1,bi,bj,2)*mrgFacW(i,j,1,bi,bj)
     &          + guNm(i,j,k2,bi,bj,2)*mrgFacW(i,j,2,bi,bj)
            guNm(i,j,k1,bi,bj,1)= 0.0
            guNm(i,j,k1,bi,bj,2)= 0.0
#else
            guNm1(i,j,k2,bi,bj) =
     &            guNm1(i,j,k1,bi,bj)*mrgFacW(i,j,1,bi,bj)
     &          + guNm1(i,j,k2,bi,bj)*mrgFacW(i,j,2,bi,bj)
            guNm1(i,j,k1,bi,bj) = 0.0
#endif
           ENDIF
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          IF ( k1SurfS(i,j,bi,bj).NE.kSurfS(i,j,bi,bj) ) THEN
           k1 = k1SurfS(i,j,bi,bj)
           k2 =  kSurfS(i,j,bi,bj)
           IF ( k2.LT.k1 .AND. k1.LE.Nr ) THEN
C-      spliting case: just copy former surface value (=k1) into new one (=k2)
            vVel(i,j,k2,bi,bj) = vVel(i,j,k1,bi,bj)
#ifdef ALLOW_ADAMSBASHFORTH_3
            gvNm(i,j,k2,bi,bj,1) = gvNm(i,j,k1,bi,bj,1)
            gvNm(i,j,k2,bi,bj,2) = gvNm(i,j,k1,bi,bj,2)
#else
            gvNm1(i,j,k2,bi,bj) = gvNm1(i,j,k1,bi,bj)
#endif
           ENDIF
           IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-      merging former (=k1) into new (=k2) surface grid cell
            vVel(i,j,k2,bi,bj) =
     &            vVel(i,j,k1,bi,bj)*mrgFacS(i,j,1,bi,bj)
     &          + vVel(i,j,k2,bi,bj)*mrgFacS(i,j,2,bi,bj)
            vVel(i,j,k1,bi,bj) = 0.0
#ifdef ALLOW_ADAMSBASHFORTH_3
            gvNm(i,j,k2,bi,bj,1) =
     &            gvNm(i,j,k1,bi,bj,1)*mrgFacS(i,j,1,bi,bj)
     &          + gvNm(i,j,k2,bi,bj,1)*mrgFacS(i,j,2,bi,bj)
            gvNm(i,j,k2,bi,bj,2) =
     &            gvNm(i,j,k1,bi,bj,2)*mrgFacS(i,j,1,bi,bj)
     &          + gvNm(i,j,k2,bi,bj,2)*mrgFacS(i,j,2,bi,bj)
            gvNm(i,j,k1,bi,bj,1)= 0.0
            gvNm(i,j,k1,bi,bj,2)= 0.0
#else
            gvNm1(i,j,k2,bi,bj) =
     &            gvNm1(i,j,k1,bi,bj)*mrgFacS(i,j,1,bi,bj)
     &          + gvNm1(i,j,k2,bi,bj)*mrgFacS(i,j,2,bi,bj)
            gvNm1(i,j,k1,bi,bj) = 0.0
#endif
           ENDIF
          ENDIF
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif /* ALLOW_SHELFICE_REMESHING */
      RETURN
      END
