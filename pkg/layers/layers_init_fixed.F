C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_init_fixed.F,v 1.2 2009/09/18 16:13:33 jmc Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_INIT_FIXED( myThid )

C ===================================================================
C     Initialize LAYERS variables that are kept fixed during the run.
C ===================================================================

      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C  INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

C  LOCAL VARIABLES:
C     k         :: loop index
C     kk,kkinit :: fine grid loop indices
C     Zf        :: depth at cell boundaries
C     Zf        :: depth at cell centers
C     dZZf      :: height of fine grid cells
C     ZZf       :: depth at cell boundaries (fine grid)
C     ZZc       :: depth at cell centers (fine grid)
C     msgBuf    :: Informational/error meesage buffer
      INTEGER k,kk,kkinit
      _RL     Zf(Nr+1)
      _RL     Zc(Nr)
      _RL     dZZf(FineGridMax)
      _RL     ZZf(FineGridMax+1)
      _RL     ZZc(FineGridMax)

      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_MNC
#ifdef LAYERS_MNC
      IF (layers_MNC) THEN
        CALL LAYERS_MNC_INIT( myThid )
      ENDIF
#endif /* LAYERS_MNC */
#endif /* ALLOW_MNC */

C  Set up the vertical grid

C     for now, just use up the entire available array for ZZ
      NZZ = FineGridMax

C     Z and ZZ are INCREASING DOWNWARD!!!
C     Maybe this is dumb but it will work as long as we are consistent


C     Each dF cell is split into FineGridFact fine cells
C     Calculate dZZf on the fine grid
      kkinit = 1
      DO k=1,Nr
        DO kk=kkinit,kkinit+FineGridFact-1
          dZZf(kk) = dRf(k) / FineGridFact
        ENDDO
        kkinit = kkinit + FineGridFact
      ENDDO

C     find the depths
      Zf(1) = 0. _d 0
      Zc(1) = drC(1)
      DO k=2,Nr
        Zf(k) = Zf(k-1) + drF(k-1)
        Zc(k) = Zc(k-1) + drC(k)
      ENDDO
      Zf(Nr+1) = Zf(Nr) + drF(Nr)

C     create ZZ
      ZZf(1) = 0. _d 0
      ZZc(1) = 0.5 _d 0 * dZZ

      DO kk=2,NZZ
            ZZf(kk) = ZZf(kk-1) + dZZf(kk-1)
            ZZc(kk) = 0.5 _d 0 * (ZZf(kk) + ZZf(kk-1))
      ENDDO
      ZZf(NZZ+1) = ZZf(NZZ) + dZZf(NZZ)

C     create the interpolating mapping arrays
      k = 1
      DO kk=1,NZZ
C       see if ZZc point is less than the top Zc point
        IF ( ZZc(kk) .LT. Zc(1) ) THEN
          MapIndex(kk) = 1
          MapFact(kk) = 1.0 _d 0
C       see if ZZc point is greater than the bottom Zc point
        ELSE IF ( (ZZc(kk) .GE. Zc(Nr)) .OR. (k .EQ. Nr) ) THEN
          MapIndex(kk) = Nr - 1
          MapFact(kk) = 0.0 _d 0
C       Otherwise we are somewhere in between and need to do interpolation)
        ELSE IF ( (ZZc(kk) .GE. Zc(k))
     &   .AND. (ZZc(kk) .LT. Zc(Nr)) ) THEN
C         Find the proper k value
          DO WHILE (ZZc(kk) .GE. Zc(k+1))
            k = k + 1
          ENDDO
C         If the loop stopped, that means Zc(k) <= ZZc(kk) < ZZc(k+1)
          MapIndex(kk) = k
          MapFact(kk) = 1.0 - (( ZZc(kk) - Zc(k) ) / drC(k+1))
        ELSE
C       This means there was a problem
          WRITE(msgBuf,'(A,I4,A,I4,A,1E14.6,A,2E14.6)')
     &     'S/R LAYERS_INIT_FIXED: kk=', kk, ', k=', k,
     &     ', ZZc(kk)=', ZZc(kk),' , Zc(k)=',Zc(k)
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R LAYERS_INIT_FIXED'
        END IF

C       See which grid box the point lies in
        IF ( ZZc(kk).LT.Zf(MapIndex(kk)+1) ) THEN
          CellIndex(kk) = MapIndex(kk)
        ELSE
          CellIndex(kk) = MapIndex(kk)+1
        END IF
      ENDDO

      IF (debugLevel .GT. 0) THEN
        CALL PRINT_MESSAGE( 'LAYERS_INIT_FIXED Debugging:',
     &      standardMessageUnit,SQUEEZE_RIGHT , 1)
        DO kk=1,NZZ
          WRITE(msgBuf,'(A,1F6.1,A,I3,A,I3,A,I3,A,1F6.4,A,I3,A,I3)')
     &     '// ZZc=', ZZc(kk),
     &     ', MapIndex(',kk,')=',MapIndex(kk),
     &     ', MapFact(',kk,')=',MapFact(kk),
     &     ', CellIndex(',kk,')=',CellIndex(kk)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
        ENDDO
      END IF

      RETURN
      END
