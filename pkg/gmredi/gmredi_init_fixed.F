C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_init_fixed.F,v 1.4 2015/10/12 20:41:26 dfer Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE GMREDI_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_INIT_FIXED
C     | o Routine to initialize GM/Redi variables
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !FUNCTIUONS:
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,k
      INTEGER bi,bj

C--   Initialize arrays in common blocks :
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+OLy
         DO i=1-Olx,sNx+Olx
           GM_isoFac2d(i,j,bi,bj) = 1. _d 0
           GM_bolFac2d(i,j,bi,bj) = 1. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef GM_K3D
C     Computing beta = df/dy
      IF ( selectCoriMap.EQ.1 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           gradf(i,j,bi,bj) =  beta
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( selectCoriMap.EQ.2 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           gradf(i,j,bi,bj) = recip_rSphere*fCoriCos(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
           gradf(i,j,bi,bj)  =  .5 _d 0*angleSinC(i,j,bi,bj)*(
     &    (fCori(i+1,j,bi,bj)-fCori(i  ,j,bi,bj))*recip_dxC(i+1,j,bi,bj)
     &   +(fCori(i  ,j,bi,bj)-fCori(i-1,j,bi,bj))*recip_dxC(i,j,bi,bj) )
     &                       +  .5 _d 0*angleCosC(i,j,bi,bj)*(
     &    (fCori(i,j+1,bi,bj)-fCori(i,j  ,bi,bj))*recip_dyC(i,j+1,bi,bj)
     &   +(fCori(i,j  ,bi,bj)-fCori(i,j-1,bi,bj))*recip_dyC(i,j,bi,bj) )
           gradf(i,j,bi,bj)=max(1. _d -18, gradf(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      CALL EXCH_XY_RL( gradf, myThid)
#endif

C--   Read horizontal 2.D scaling factors from files:
      IF ( GM_iso2dFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( GM_iso2dFile, ' ', GM_isoFac2d, 0, myThid )
       CALL EXCH_XY_RS( GM_isoFac2d, myThid )
      ENDIF
      IF ( GM_bol2dFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( GM_bol2dFile, ' ', GM_bolFac2d, 0, myThid )
       CALL EXCH_XY_RS( GM_bolFac2d, myThid )
      ENDIF

C--   Set vertical 1.D scaling factors
      _BEGIN_MASTER( myThid )

      DO k=1,Nr
        GM_isoFac1d(k) = 1. _d 0
        GM_bolFac1d(k) = 1. _d 0
      ENDDO

C-    Read vertical 1.D scaling factors from files:
      IF ( GM_iso1dFile .NE. ' ' ) THEN
        CALL READ_GLVEC_RS( GM_iso1dFile, ' ',
     &                      GM_isoFac1d, Nr, 1, myThid )
      ENDIF
      IF ( GM_bol1dFile .NE. ' ' ) THEN
        CALL READ_GLVEC_RS( GM_bol1dFile, ' ',
     &                      GM_bolFac1d, Nr, 1, myThid )
      ENDIF
      _END_MASTER( myThid )

C-    Everyone else must wait for arrays to be loaded
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL GMREDI_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL GMREDI_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
