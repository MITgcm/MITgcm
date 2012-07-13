C $Header: /u/gcmpack/MITgcm/model/src/ini_global_domain.F,v 1.1 2012/07/13 22:07:09 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_GLOBAL_DOMAIN
C     !INTERFACE:
      SUBROUTINE INI_GLOBAL_DOMAIN( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_GLOBAL_DOMAIN
C     | o Initialise domain (i.e., where there is fluid)
C     |   related (global) quantities.
C     |   Called after grid and masks are set (ini_grid,
C     |   ini_masks) or modified (packages_init_fixed call).
C     *==========================================================*
C     | Compute global domain Area ;
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: my Thread Id number
      INTEGER myThid

C     == Local variables in common ==
      _RL tileArea(nSx,nSy), threadArea
C     put tileArea in (local) common block to print from master-thread:
      COMMON / LOCAL_INI_GLOB_DOMAIN / tileArea

C     !LOCAL VARIABLES:
C     === Local variables ===
C     bi,bj  :: tile indices
C     i, j   :: Loop counters
      INTEGER bi, bj
      INTEGER i, j, nCorners
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif /* ALLOW_EXCH2 */
CEOP

C--   Initialisation

#ifdef NONLIN_FRSURF
C--   Save initial geometrical hFac factor into h0Fac (fixed in time):
C     better here (after packages_init_fixed call) than in INI_MASKS_ETC
C     in case 1 pkg would need to modify them.
C    <= moved to INI_MASK_ETC , despite comment above, since:
C      a) in case 1 pkg is changing hFac, this pkg should also update h0Fac
C      b) pkg/shelfice does modify hFac but done directly within ini_masks_etc
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Calculate global domain area:
C     use to be in ini_masks_etc.F but has been move after packages_init_fixed
C     in case 1 pkg (e.g., OBCS) modifies the domain size.
      threadArea = 0. _d 0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      Compute the domain Area:
        tileArea(bi,bj) = 0. _d 0
        DO j=1,sNy
         DO i=1,sNx
          tileArea(bi,bj) = tileArea(bi,bj)
     &                    + rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
         ENDDO
        ENDDO
c       threadArea = threadArea + tileArea(bi,bj)
       ENDDO
      ENDDO
c#ifdef ALLOW_AUTODIFF_TAMC
C_jmc: apply GLOBAL_SUM to thread-local variable (not in common block)
c      _GLOBAL_SUM_RL( threadArea, myThid )
c#else
      CALL GLOBAL_SUM_TILE_RL( tileArea, threadArea, myThid )
c#endif
      _BEGIN_MASTER( myThid )
      globalArea = threadArea
C-    list empty tiles:
      msgBuf(1:1) = ' '
      DO bj = 1,nSy
       DO bi = 1,nSx
        IF ( tileArea(bi,bj).EQ.0. _d 0 ) THEN
#ifdef ALLOW_EXCH2
         WRITE(msgBuf,'(A,I6,A,2I4,A)')
     &    'Empty tile: #', W2_myTileList(bi,bj), ' (bi,bj=',bi,bj,' )'
#else
         WRITE(msgBuf,'(A,I6,I6)') 'Empty tile bi,bj=', bi, bj
#endif
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDDO
      ENDDO
      IF ( msgBuf(1:1).NE.' ' ) THEN
         WRITE(msgBuf,'(A)') ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
      ENDIF
      _END_MASTER( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   With Cubed-Sphere Exchanges, check if CS-corners are part of the domain
      IF ( useCubedSphereExchange ) THEN
        nCorners = 0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_EXCH2
           myTile = W2_myTileList(bi,bj)
           southWestCorner = exch2_isWedge(myTile).EQ.1
     &                 .AND. exch2_isSedge(myTile).EQ.1
           southEastCorner = exch2_isEedge(myTile).EQ.1
     &                 .AND. exch2_isSedge(myTile).EQ.1
           northEastCorner = exch2_isEedge(myTile).EQ.1
     &                 .AND. exch2_isNedge(myTile).EQ.1
           northWestCorner = exch2_isWedge(myTile).EQ.1
     &                 .AND. exch2_isNedge(myTile).EQ.1
#else /* ALLOW_EXCH2 */
           southWestCorner = .TRUE.
           southEastCorner = .TRUE.
           northWestCorner = .TRUE.
           northEastCorner = .TRUE.
#endif /* ALLOW_EXCH2 */
           IF ( southWestCorner .AND. kSurfC( 1 , 1 ,bi,bj).LE.Nr )
     &       nCorners = nCorners + 1
           IF ( southEastCorner .AND. kSurfC(sNx, 1 ,bi,bj).LE.Nr )
     &       nCorners = nCorners + 1
           IF ( northWestCorner .AND. kSurfC( 1 ,sNy,bi,bj).LE.Nr )
     &       nCorners = nCorners + 1
           IF ( northEastCorner .AND. kSurfC(sNx,sNy,bi,bj).LE.Nr )
     &       nCorners = nCorners + 1
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_INT( nCorners, myThid )
        _BEGIN_MASTER( myThid )
        IF ( nCorners.GE.1 ) hasWetCSCorners = .TRUE.
        WRITE(msgBuf,'(A,I4,A)') 'INI_GLOBAL_DOMAIN: Found',
     &                 nCorners, ' CS-corner Pts in the domain'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
      ENDIF

C--   Everyone else must wait for global-domain parameters to be set
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
