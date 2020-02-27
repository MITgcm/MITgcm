#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_REMESH_C_MASK
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_C_MASK(
     O                    k1SurfC, mrgFacC,
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_C_MASK
C     | o Loops through top level cells and determines those where
C     |   hFac is too large and hence splits into two cells,
C     |   and cells where hFac is too small, and merges cell with
C     |   below.
C     | o Update surface ref position Ro_surf and anomaly Eta
C     |   as well as R_shelfIce.
C     | o Recomputes h0FacC using formula from initialisation
C     |   based on new R_shelfIce
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     k1SurfC   :: surface level index (at cell center) before remeshing
C     mrgFacC   :: merging weight for tracers (cell center)
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      INTEGER k1SurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mrgFacC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE_REMESHING
#ifdef NONLIN_FRSURF
C     !LOCAL VARIABLES:
C     bi,bj   :: tile indices
C     i,j,k   :: Loop counters
C     k1, k2  :: previous and new surface level
C     ioUnit  :: temp for writing msg unit
C     msgBuf  :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j
      INTEGER ks, k1, k2
#ifdef SHELFICE_REMESH_PRINT
      LOGICAL prtFirst, prtPoint
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      _RL sync_fac, stag_fac
      _RL adjust, r_newDz, tmpVar

#ifdef SHELFICE_REMESH_PRINT
      prtFirst = .TRUE.
      ioUnit = -1
      IF ( debugLevel.GE.debLevB ) ioUnit = standardMessageUnit
#endif
      IF ( staggerTimestep ) THEN
        sync_fac = 0.0
        stag_fac = 1.0
      ELSE
        sync_fac = 1.0
        stag_fac = 0.0
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          k1SurfC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
          mrgFacC(i,j,1,bi,bj) = 0.
          mrgFacC(i,j,2,bi,bj) = 0.
          ks = kTopC(i,j,bi,bj)

          IF ( ks.NE.0 ) THEN
C-- SPLIT CELLS
           IF ( ks.GT.1 .AND.
     &          hFac_surfC(i,j,bi,bj).GT.SHELFICEsplitThreshold ) THEN
            IF ( (hFac_surfC(i,j,bi,bj)-1)*drF(ks)*recip_drF(ks-1)
     &           .GT.SHELFICEmergeThreshold ) THEN

             k1 = ks
             k2 = ks-1
             kSurfC(i,j,bi,bj) = k2
             adjust = rF(k2) - R_shelfIce(i,j,bi,bj)
#ifdef SHELFICE_REMESH_PRINT
             prtPoint = ( ioUnit.GE.0 ) .AND.
     &          ( i.GE.1 .AND. i.LE.sNx .AND. j.GE.1 .AND. j.LE.sNy )
             IF ( prtPoint ) THEN
              IF ( prtFirst ) THEN
               WRITE(msgBuf,'(A,I10)') 'SHI_REMESH at it=', myIter
               CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
               prtFirst = .FALSE.
              ENDIF
              WRITE(msgBuf,'(A,2I5,2I4,A,1P2E12.3)') '--> REMESH in:',
     &            i, j, bi, bj, ' , x,y=', XC(i,j,bi,bj),YC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' before:',
     &        '  ks=', k1, ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=', hFac_surfC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif /* SHELFICE_REMESH_PRINT */
C-    decrement eta and increment R_shelfIce and Ro_surf
             etaN(i,j,bi,bj) = etaN(i,j,bi,bj)- adjust
             etaH(i,j,bi,bj) = etaH(i,j,bi,bj)- adjust
             etaHnm1(i,j,bi,bj) = etaHnm1(i,j,bi,bj)- adjust
             R_shelfIce(i,j,bi,bj) = R_shelfIce(i,j,bi,bj)+adjust
             Ro_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)+adjust
C     update cell-centered grid factors and mask: maskC, h0FacC, recip_hFacC
             maskC(i,j,k2,bi,bj)  = oneRS
             h0FacC(i,j,k1,bi,bj) = oneRS
             h0FacC(i,j,k2,bi,bj) = oneRS
             recip_hFacC(i,j,k1,bi,bj) = oneRS
             recip_hFacC(i,j,k2,bi,bj) = oneRS
             IF ( k1 .EQ. kLowC(i,j,bi,bj) ) THEN
               h0FacC(i,j,k1,bi,bj) = ( rF(k1) - R_low(i,j,bi,bj) )
     &                              *recip_drF(k1)
               recip_hFacC(i,j,k1,bi,bj) = oneRS / h0FacC(i,j,k1,bi,bj)
             ENDIF
#ifdef SHELFICE_REMESH_PRINT
             IF ( prtPoint ) THEN
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' after :',
     &        '  ks=', k2, ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=',
     &           h0FacC(i,j,k2,bi,bj)+etaH(i,j,bi,bj)*recip_drF(k2)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif /* SHELFICE_REMESH_PRINT */
            ENDIF
           ENDIF
          ENDIF

C-- MERGE CELLS
          IF ( ks.NE.0 .AND. ks.LT.kLowC (i,j,bi,bj) ) THEN
           IF ( hFac_surfC(i,j,bi,bj).LT.SHELFICEmergeThreshold ) THEN
            IF ( (hFac_surfC(i,j,bi,bj)*drF(ks)*recip_drF(ks+1)+1)
     &           .LT.SHELFICEsplitThreshold ) THEN

             k1 = ks
             k2 = ks+1
             kSurfC(i,j,bi,bj) = k2
             adjust = R_shelfIce(i,j,bi,bj)-rF(k2)
#ifdef SHELFICE_REMESH_PRINT
             prtPoint = ( ioUnit.GE.0 ) .AND.
     &          ( i.GE.1 .AND. i.LE.sNx .AND. j.GE.1 .AND. j.LE.sNy )
             IF ( prtPoint ) THEN
              IF ( prtFirst ) THEN
               WRITE(msgBuf,'(A,I10)') 'SHI_REMESH at it=', myIter
               CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
               prtFirst = .FALSE.
              ENDIF
              WRITE(msgBuf,'(A,2I5,2I4,A,1P2E12.3)') '--> REMESH in:',
     &            i, j, bi, bj, ' , x,y=', XC(i,j,bi,bj),YC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' before:',
     &        '  ks=', k1, ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=', hFac_surfC(i,j,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif /* SHELFICE_REMESH_PRINT */
C-    increment eta and decrement R_shelfIce and Ro_surf
             etaN(i,j,bi,bj) = etaN(i,j,bi,bj) + adjust
             etaH(i,j,bi,bj) = etaH(i,j,bi,bj) + adjust
             etaHnm1(i,j,bi,bj) = etaHnm1(i,j,bi,bj) + adjust
             R_shelfIce(i,j,bi,bj) = R_shelfIce(i,j,bi,bj)-adjust
             Ro_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - adjust

C-    Compute merging weights for tracer in new (=k2) top cell, accounting
C     for former (=k1) top cell content:
C      Tr(k2) <-- mrgFacC(1)*Tr(k1) + mrgFacC(2)*Tr(k2) ; Tr(k1) <-- 0.
C     first find the inverse thickness of the new top cell:
             IF ( k2 .LT. kLowC(i,j,bi,bj) ) THEN
              r_newDz = oneRL / ( drF(k2) + etaH(i,j,bi,bj) )
             ELSE
              r_newDz = oneRL
     &                / ( rF(k2)-r_low(i,j,bi,bj) + etaH(i,j,bi,bj) )
             ENDIF
             mrgFacC(i,j,1,bi,bj) = ( stag_Fac*hFac_surfC(i,j,bi,bj)
     &                              + sync_fac*hFacC(i,j,k1,bi,bj)
     &                              )*drF(k1)*r_newDz
             mrgFacC(i,j,2,bi,bj) = hFacC(i,j,k2,bi,bj)*drF(k2)*r_newDz
C     update cell-centered grid factors and mask: maskC, h0FacC, recip_hFacC
             maskC(i,j,k1,bi,bj)  = zeroRS
             h0FacC(i,j,k1,bi,bj) = zeroRS
             h0FacC(i,j,k2,bi,bj) = oneRS
             recip_hFacC(i,j,k1,bi,bj) = zeroRS
             recip_hFacC(i,j,k2,bi,bj) = oneRS
             IF ( k2 .EQ. kLowC(i,j,bi,bj) ) THEN
               h0FacC(i,j,k2,bi,bj) = ( rF(k2) - R_low(i,j,bi,bj) )
     &                              * recip_drF(k2)
               recip_hFacC(i,j,k2,bi,bj) = oneRS / h0FacC(i,j,k2,bi,bj)
             ENDIF
#ifdef SHELFICE_REMESH_PRINT
             IF ( prtPoint ) THEN
              WRITE(msgBuf,'(2A,I4,3(A,1P1E12.3))') ' after :',
     &        '  ks=', k2, ' Ro_s=', Ro_surf(i,j,bi,bj),
     &        ' eta=', etaH(i,j,bi,bj), ' hFac=',
     &           h0FacC(i,j,k2,bi,bj)+etaH(i,j,bi,bj)*recip_drF(k2)
              CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
             ENDIF
#endif /* SHELFICE_REMESH_PRINT */
            ENDIF
           ENDIF
          ENDIF

C-  Update remaining cell-centered grid fields
C   Note: CALC_SURF_DR & UPDATE_SURF_DR will update hFac_surfC and surface hFacC
          IF ( kSurfC(i,j,bi,bj).NE.k1SurfC(i,j,bi,bj) ) THEN
            k1 = k1SurfC(i,j,bi,bj)
            k2 =  kSurfC(i,j,bi,bj)
            hFacC(i,j,k1,bi,bj) = h0FacC(i,j,k1,bi,bj)
            hFacC(i,j,k2,bi,bj) = h0FacC(i,j,k2,bi,bj)
            tmpVar = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
            recip_Rcol(i,j,bi,bj) = 1. _d 0 / tmpVar
            kTopC(i,j,bi,bj)  = k2
          ENDIF

         ENDDO
        ENDDO
C---
       ENDDO
      ENDDO
#ifdef SHELFICE_REMESH_PRINT
      IF ( ioUnit.GE.0 .AND. .NOT. prtFirst ) THEN
        WRITE(msgBuf,'(A,A)') 'SHI_REMESH :', ' end of report'
        CALL PRINT_MESSAGE( msgBuf,ioUnit,SQUEEZE_RIGHT,myThid )
      ENDIF
#endif /* SHELFICE_REMESH_PRINT */

#endif /* NONLIN_FRSURF */
#endif /* ALLOW_SHELFICE_REMESHING */
      RETURN
      END
