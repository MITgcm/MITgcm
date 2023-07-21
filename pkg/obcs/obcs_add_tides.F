#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_ADD_TIDES

C     !INTERFACE:
      SUBROUTINE OBCS_ADD_TIDES( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_ADD_TIDES
C     | o Modify OB normal flow to add tidal forcing
C     |   NOTE that at the moment tidal forcing is applied
C     |   only to "normal" flow.  Code below should eventually
C     |   be augmented to also specify flow parallel to boundary.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_TIDES
C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     bi, bj       :: tile indices
C     i,j,k        :: loop indices
C     iB, jB       :: local index of open boundary
C     msgBuf       :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, k, iB, jB
      INTEGER td
      _RL  timePhase, timeCos, timeSin
      _RL  uTide, vTide

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_ADD_TIDES',myThid)
#endif

C--   Add tidal currents:
      DO td=1,OBCS_nTidalComp
       IF ( OBCS_tidalPeriod(td) .NE. zeroRL ) THEN

        timePhase = myTime * twoRL * PI / OBCS_tidalPeriod(td)
        timeCos = COS( timePhase )
        timeSin = SIN( timePhase )
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef OLD_OBCS_TIDES
#ifdef ALLOW_OBCS_EAST
          IF ( tileHasOBE(bi,bj) ) THEN
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             iB = OB_Ie(j,bi,bj)
             IF ( iB.NE.OB_indexNone ) THEN
              OBEu(j,k,bi,bj) = OBEu(j,k,bi,bj) +
     &              maskW(iB,j,k,bi,bj) * OBE_uTideCs(j,td,bi,bj) *
     &              COS( 2.D0 * PI * (myTime-OBE_uTideSn(j,td,bi,bj)) /
     &                   OBCS_tidalPeriod(td) )
             ENDIF
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
          IF ( tileHasOBW(bi,bj) ) THEN
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             iB = OB_Iw(j,bi,bj)
             IF ( iB.NE.OB_indexNone ) THEN
              OBWu(j,k,bi,bj) = OBWu(j,k,bi,bj) +
     &              maskW(1+iB,j,k,bi,bj) * OBW_uTideCs(j,td,bi,bj) *
     &              COS( 2.D0 * PI * (myTime-OBW_uTideSn(j,td,bi,bj)) /
     &                   OBCS_tidalPeriod(td) )
             ENDIF
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
          IF ( tileHasOBN(bi,bj) ) THEN
           DO k=1,Nr
            DO i=1-OLx,sNx+OLx
             jB = OB_Jn(i,bi,bj)
             IF ( jB.NE.OB_indexNone ) THEN
              OBNv(i,k,bi,bj) = OBNv(i,k,bi,bj) +
     &              maskS(i,jB,k,bi,bj) * OBN_vTideCs(i,td,bi,bj) *
     &              COS( 2.D0 * PI * (myTime-OBN_vTideSn(i,td,bi,bj)) /
     &                   OBCS_tidalPeriod(td) )
             ENDIF
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
          IF ( tileHasOBS(bi,bj) ) THEN
           DO k=1,Nr
            DO i=1-OLx,sNx+OLx
             jB = OB_Js(i,bi,bj)
             IF ( jB.NE.OB_indexNone ) THEN
              OBSv(i,k,bi,bj) = OBSv(i,k,bi,bj) +
     &              maskS(i,1+jB,k,bi,bj)* OBS_vTideCs(i,td,bi,bj) *
     &              COS( 2.D0 * PI * (myTime-OBS_vTideSn(i,td,bi,bj)) /
     &                   OBCS_tidalPeriod(td) )
             ENDIF
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#else /* OLD_OBCS_TIDES */

#ifdef ALLOW_OBCS_EAST
          IF ( tileHasOBE(bi,bj) ) THEN
           DO j=1-OLy,sNy+OLy
            iB = OB_Ie(j,bi,bj)
            IF ( iB.NE.OB_indexNone ) THEN
             uTide = OBE_uTideCs(j,td,bi,bj) * timeCos
     &             + OBE_uTideSn(j,td,bi,bj) * timeSin
             vTide = OBE_vTideCs(j,td,bi,bj) * timeCos
     &             + OBE_vTideSn(j,td,bi,bj) * timeSin
             DO k=1,Nr
              OBEu(j,k,bi,bj) = OBEu(j,k,bi,bj)
     &                        + uTide * maskW(iB,j,k,bi,bj)
              OBEv(j,k,bi,bj) = OBEv(j,k,bi,bj)
     &                        + vTide * maskS(iB,j,k,bi,bj)
             ENDDO
            ENDIF
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
          IF ( tileHasOBW(bi,bj) ) THEN
           DO j=1-OLy,sNy+OLy
            iB = OB_Iw(j,bi,bj)
            IF ( iB.NE.OB_indexNone ) THEN
             uTide = OBW_uTideCs(j,td,bi,bj) * timeCos
     &             + OBW_uTideSn(j,td,bi,bj) * timeSin
             vTide = OBW_vTideCs(j,td,bi,bj) * timeCos
     &             + OBW_vTideSn(j,td,bi,bj) * timeSin
             DO k=1,Nr
              OBWu(j,k,bi,bj) = OBWu(j,k,bi,bj)
     &                        + uTide * maskW(1+iB,j,k,bi,bj)
              OBWv(j,k,bi,bj) = OBWv(j,k,bi,bj)
     &                        + vTide * maskS(iB,j,k,bi,bj)
             ENDDO
            ENDIF
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
          IF ( tileHasOBN(bi,bj) ) THEN
           DO i=1-OLx,sNx+OLx
            jB = OB_Jn(i,bi,bj)
            IF ( jB.NE.OB_indexNone ) THEN
             uTide = OBN_uTideCs(i,td,bi,bj) * timeCos
     &             + OBN_uTideSn(i,td,bi,bj) * timeSin
             vTide = OBN_vTideCs(i,td,bi,bj) * timeCos
     &             + OBN_vTideSn(i,td,bi,bj) * timeSin
             DO k=1,Nr
              OBNu(i,k,bi,bj) = OBNu(i,k,bi,bj)
     &                        + uTide * maskW(i,jB,k,bi,bj)
              OBNv(i,k,bi,bj) = OBNv(i,k,bi,bj)
     &                        + vTide * maskS(i,jB,k,bi,bj)
             ENDDO
            ENDIF
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
          IF ( tileHasOBS(bi,bj) ) THEN
           DO i=1-OLx,sNx+OLx
            jB = OB_Js(i,bi,bj)
            IF ( jB.NE.OB_indexNone ) THEN
             uTide = OBS_uTideCs(i,td,bi,bj) * timeCos
     &             + OBS_uTideSn(i,td,bi,bj) * timeSin
             vTide = OBS_vTideCs(i,td,bi,bj) * timeCos
     &             + OBS_vTideSn(i,td,bi,bj) * timeSin
             DO k=1,Nr
              OBSu(i,k,bi,bj) = OBSu(i,k,bi,bj)
     &                        + uTide * maskW(i,jB,k,bi,bj)
              OBSv(i,k,bi,bj) = OBSv(i,k,bi,bj)
     &                        + vTide * maskS(i,1+jB,k,bi,bj)
             ENDDO
            ENDIF
           ENDDO
          ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#endif /* OLD_OBCS_TIDES */

C    end bi,bj loops
         ENDDO
        ENDDO

C    end if (tidalPeriod <> 0) and end td (= tidal comp. index) do-loop
       ENDIF
      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_ADD_TIDES',myThid)
#endif

#endif /* ALLOW_OBCS_TIDES */
#endif /* ALLOW_OBCS */

      RETURN
      END
