C $Header: /u/gcmpack/MITgcm/pkg/down_slope/dwnslp_init_fixed.F,v 1.3 2011/06/07 21:05:13 jmc Exp $
C $Name:  $

#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE DWNSLP_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DWNSLP_INIT_FIXED
C     | o Routine to initialize Down-Sloping arrays ;
C     |   find potential location of Down-Sloping flow.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DWNSLP_SIZE.h"
#include "DWNSLP_PARAMS.h"
#include "DWNSLP_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_DOWN_SLOPE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     logFname,STATUS='UNKNOWN')
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(19) logFname
      INTEGER i, j, k
      INTEGER bi, bj
      INTEGER n, ncount, ijd, ijr
      INTEGER ideep, jdeep, kdeep, dkMx
      INTEGER ishelf,jshelf,kshelf
      INTEGER downward
      _RL     dz_bottom
      _RL     drFlowMin
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize common bloc arrays :

        DWNSLP_NbSite(bi,bj) = 0
        DO n=1,DWNSLP_size
         DWNSLP_ijDeep(n,bi,bj) = 0
         DWNSLP_shVsD(n,bi,bj)  = 0
         DWNSLP_deepK(n,bi,bj)  = 0
         DWNSLP_Gamma(n,bi,bj)  = 0.
         DWNSLP_Transp(n,bi,bj) = 0.
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---- set list of bathymetric step (= potential location of Down-Sloping flow)
        ncount = 0

        IF ( gravitySign.GT.0. ) THEN
C--   gravity > 0 (p-Coord)

C-    in X direction (U-flow):
         DO j=1,sNy
          DO i=1,sNx+1
           IF ( kSurfW(i,j,bi,bj).LE.Nr ) THEN

            IF ( kSurfC(i,j,bi,bj).LT.kSurfC(i-1,j,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i+OLx-1) + (j+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = -1
             ENDIF
            ENDIF

            IF ( kSurfC(i,j,bi,bj).GT.kSurfC(i-1,j,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i-1+OLx-1) + (j+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = 1
             ENDIF
            ENDIF

           ENDIF
          ENDDO
         ENDDO

C-    in Y direction (V-flow):

         DO j=1,sNy+1
          DO i=1,sNx
           IF (  kSurfS(i,j,bi,bj).LE.Nr ) THEN

            IF ( kSurfC(i,j,bi,bj).LT.kSurfC(i,j-1,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i+OLx-1) + (j+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = -xSize
             ENDIF
            ENDIF

            IF ( kSurfC(i,j,bi,bj).GT.kSurfC(i,j-1,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i+OLx-1) + (j-1+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = xSize
             ENDIF
            ENDIF

           ENDIF
          ENDDO
         ENDDO

        ELSE
C--   gravity < 0 (z-Coord)

C-    in X direction (U-flow):

         DO j=1,sNy
          DO i=1,sNx+1
           IF ( kSurfW(i,j,bi,bj).LE.Nr ) THEN

            IF ( kLowC(i,j,bi,bj).GT.kLowC(i-1,j,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i+OLx-1) + (j+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = -1
             ENDIF
            ENDIF

            IF ( kLowC(i,j,bi,bj).LT.kLowC(i-1,j,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i-1+OLx-1) + (j+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = 1
             ENDIF
            ENDIF

           ENDIF
          ENDDO
         ENDDO

C-    in Y direction (V-flow):

         DO j=1,sNy+1
          DO i=1,sNx
           IF (  kSurfS(i,j,bi,bj).LE.Nr ) THEN

            IF ( kLowC(i,j,bi,bj).GT.kLowC(i,j-1,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i+OLx-1) + (j+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = -xSize
             ENDIF
            ENDIF

            IF ( kLowC(i,j,bi,bj).LT.kLowC(i,j-1,bi,bj) ) THEN
             ncount = ncount + 1
             IF ( ncount.LE.DWNSLP_size ) THEN
              DWNSLP_ijDeep(ncount,bi,bj) =
     &          1 + (i+OLx-1) + (j-1+OLy-1)*xSize
              DWNSLP_shVsD(ncount,bi,bj) = xSize
             ENDIF
            ENDIF

           ENDIF
          ENDDO
         ENDDO

C--     end if gravitySign block
        ENDIF

C-    Store the Nb of bathymetric steps (=maximum Nb of Downsloping-flow site)
        DWNSLP_NbSite(bi,bj) = ncount

C-    Check dimension :
        IF (ncount.GT.DWNSLP_size) THEN
          WRITE(msgBuf,'(A,I8,A)')
     &      ' DWNSLP_INIT: DWNSLP_size=',DWNSLP_size,' too small !'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,2I4,A,I8)')
     &      ' DWNSLP_INIT: min needed for tile',bi,bj,' :', ncount
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R DWNSLP_INIT'
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Compute geometric factor Gamma = slope * effective cross section area
        DO n=1,DWNSLP_NbSite(bi,bj)

          ijd = DWNSLP_ijDeep(n,bi,bj)
          ideep = 1-OLx + MOD(ijd-1,xSize)
          jdeep = 1-Oly + (ijd-1)/xSize
          ijr = DWNSLP_shVsD(n,bi,bj)
          ishelf = ideep + MOD(ijr,xSize)
          jshelf = jdeep + ijr/xSize
          IF ( usingPCoords ) THEN
            kdeep  = kSurfC(ideep, jdeep, bi,bj)
            kshelf = kSurfC(ishelf,jshelf,bi,bj)
            downward = -1
          ELSE
            kdeep  = kLowC (ideep, jdeep, bi,bj)
            kshelf = kLowC (ishelf,jshelf,bi,bj)
            downward = 1
          ENDIF

          i= MAX(ideep,ishelf)
          j= MAX(jdeep,jshelf)

C--   calculate the minimum level thickness between kshelf & kdeep:
          drFlowMin = DWNSLP_drFlow
          DO k = kshelf,kdeep,downward
            drFlowMin = MIN( drFlowMin,
     &                       drF(k)*hFacC(ideep,jdeep,k,bi,bj) )
          ENDDO

          IF (DWNSLP_slope.NE.0.) THEN
C--   Use fixed slope = DWNSLP_slope :
           IF (ABS(ijr).EQ.1) THEN
C-    slope along X dir:
            DWNSLP_Gamma(n,bi,bj) = DWNSLP_slope*dyG(i,j,bi,bj)
     &       *MIN( drF(kshelf)*hFacW(i,j,kshelf,bi,bj), drFlowMin )
           ELSE
C-    slope along Y dir:
            DWNSLP_Gamma(n,bi,bj) = DWNSLP_slope*dxG(i,j,bi,bj)
     &       *MIN( drF(kshelf)*hFacS(i,j,kshelf,bi,bj), drFlowMin )
           ENDIF
          ELSE
C--   Compute and use the local slope :
           IF ( usingPCoords ) THEN
            dz_bottom = Ro_surf(ideep,jdeep,bi,bj)
     &                - Ro_surf(ishelf,jshelf,bi,bj)
C     a quick way to convert Delta.P to Delta.Z :
            dz_bottom = dz_bottom*recip_gravity*recip_rhoConst
           ELSE
            dz_bottom = R_low(ishelf,jshelf,bi,bj)
     &                - R_low(ideep,jdeep,bi,bj)
           ENDIF
           IF (ABS(ijr).EQ.1) THEN
C-    slope along X dir:
            DWNSLP_Gamma(n,bi,bj) = dz_bottom*recip_dxC(i,j,bi,bj)
     &       *dyG(i,j,bi,bj)
     &       *MIN( drF(kshelf)*hFacW(i,j,kshelf,bi,bj), drFlowMin )
           ELSE
C-    slope along Y dir:
            DWNSLP_Gamma(n,bi,bj) = dz_bottom*recip_dyC(i,j,bi,bj)
     &       *dxG(i,j,bi,bj)
     &       *MIN( drF(kshelf)*hFacS(i,j,kshelf,bi,bj), drFlowMin )
           ENDIF

          ENDIF

        ENDDO

C-    end bi,bj loops.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C- Print usefull variables :
      _BARRIER
      _BEGIN_MASTER(myThid)

      DWNSLP_ioUnit = 0
      IF ( debugLevel.GE.debLevA ) THEN
        CALL MDSFINDUNIT( DWNSLP_ioUnit, myThid )
      ENDIF
      IF ( DWNSLP_ioUnit.GT.0 ) THEN
        WRITE(logFname,'(A11,I4.4,A4)') 'down_slope.',myProcId,'.log'
        OPEN(DWNSLP_ioUnit,FILE=logFname,STATUS='UNKNOWN')
      ENDIF

      DO bj = 1,nSy
       DO bi = 1,nSx

        WRITE(msgBuf,'(A,2I4,I8)')
     &      'DWNSLP_INIT: DWNSLP_NbSite=',bi,bj,DWNSLP_NbSite(bi,bj)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C---
        IF ( DWNSLP_ioUnit.GT.0 ) THEN
         WRITE(DWNSLP_ioUnit,'(A,2I4,2I8)')
     &     ' DWNSLP_INIT: bi,bj, DWNSLP_NbSite, xSize =',
     &        bi,bj, DWNSLP_NbSite(bi,bj), xSize
         WRITE(DWNSLP_ioUnit,'(A)')
     &   '  bi  bj     n :     ijd  is  js ,   ijr  ks dkMx  Gamma :'
         DO n=1,DWNSLP_NbSite(bi,bj)
          ijd = DWNSLP_ijDeep(n,bi,bj)
          ideep = 1-OLx + MOD(ijd-1,xSize)
          jdeep = 1-Oly + (ijd-1)/xSize
          ijr = DWNSLP_shVsD(n,bi,bj)
          ishelf = ideep + MOD(ijr,xSize)
          jshelf = jdeep + ijr/xSize
          IF ( usingPCoords ) THEN
            kshelf = kSurfC(ishelf,jshelf,bi,bj)
            dkMx = kshelf - kSurfC(ideep,jdeep,bi,bj)
          ELSE
            kshelf = kLowC (ishelf,jshelf,bi,bj)
            dkMx = kLowC (ideep,jdeep,bi,bj) - kshelf
          ENDIF
          WRITE(DWNSLP_ioUnit,'(2I4,I6,A,I8,2I4,A,I6,2I4,1PE14.6)')
     &      bi,bj,n, ' :', ijd, ideep, jdeep,
     &      ' ,', ijr, kshelf, dkMx, DWNSLP_Gamma(n,bi,bj)
         ENDDO
         WRITE(DWNSLP_ioUnit,*)
        ENDIF
C---
       ENDDO
      ENDDO
      IF ( DWNSLP_ioUnit.GT.0 .AND. debugLevel.LT.debLevD ) THEN
        CLOSE(DWNSLP_ioUnit)
        DWNSLP_ioUnit = 0
      ENDIF

      _END_MASTER(myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DWNSLP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DOWN_SLOPE */
      RETURN
      END
