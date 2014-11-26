C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_set_calc.F,v 1.3 2014/11/26 20:48:32 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: DIAGNOSTICS_SET_CALC

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SET_CALC( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R DIAGNOSTICS_SET_CALC
C     |  Set parameters and variables used in post-processing
C     |      diagnostics
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_CALC.h"
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif /* ALLOW_OBCS */

C     !INPUT PARAMETERS:
C     myThid     ::  my thread Id number
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i, j
      INTEGER biG, bjG
      _RL     dxLoc, dyLoc, d2Loc, d2Min
      _RL     xLine, xy0, xyLoc, xyMin
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_OBCS
      LOGICAL kPsi(1:sNx+1,1:sNy+1,nSx,nSy)
#endif /* ALLOW_OBCS */

C--   Set indices of grid-point location where Psi == 0
      IF ( xPsi0.EQ.UNSET_RS .OR. yPsi0.EQ.UNSET_RS ) THEN
C-    Set indices to (-1,0) = disabled value
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           iPsi0(bi,bj) = -1
           jPsi0(bi,bj) =  0
         ENDDO
        ENDDO
      ELSE
#ifdef ALLOW_OBCS
C-     set flag where Psi is computed
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy+1
           DO i = 1,sNx+1
             kPsi(i,j,bi,bj) = .TRUE.
           ENDDO
          ENDDO
          IF ( useOBCS ) THEN
           DO j = 1,sNy+1
            DO i = 1,sNx+1
             kPsi(i,j,bi,bj) = OBCS_insideMask( i , j ,bi,bj).EQ.oneRS
     &                    .OR. OBCS_insideMask(i-1, j ,bi,bj).EQ.oneRS
     &                    .OR. OBCS_insideMask( i ,j-1,bi,bj).EQ.oneRS
     &                    .OR. OBCS_insideMask(i-1,j-1,bi,bj).EQ.oneRS
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
#endif /* ALLOW_OBCS */
C-      find minimum distance:
        d2Min = -1. _d 0
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy+1
           DO i = 1,sNx+1
             dxLoc = xG(i,j,bi,bj)-xPsi0
             dyLoc = yG(i,j,bi,bj)-yPsi0
             d2Loc = dxLoc*dxLoc + dyLoc*dyLoc
#ifdef ALLOW_OBCS
             IF ((d2Loc.LT.d2Min .OR. d2Min.EQ.-1. _d 0)
     &                            .AND. kPsi(i,j,bi,bj) ) d2Min = d2Loc
#else
             IF ( d2Loc.LT.d2Min .OR. d2Min.EQ.-1. _d 0 ) d2Min = d2Loc
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        d2Min = -d2Min
        _GLOBAL_MAX_RL( d2Min, myThid )
        d2Min = -d2Min
C-      find list of grid-points at minimum distance:
        xyMin = 0.
        xLine = (sNx+1)*nSx*nPx
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          iPsi0(bi,bj) = 0
          jPsi0(bi,bj) = 0
          biG = bi-1+(myXGlobalLo-1)/sNx
          bjG = bj-1+(myYGlobalLo-1)/sNy
          xy0 = biG*(sNx+1) + bjG*(sNy+1)*xLine
          DO j = 1,sNy+1
           DO i = 1,sNx+1
             dxLoc = xG(i,j,bi,bj)-xPsi0
             dyLoc = yG(i,j,bi,bj)-yPsi0
             d2Loc = dxLoc*dxLoc + dyLoc*dyLoc
#ifdef ALLOW_OBCS
             IF ( d2Loc.EQ.d2Min .AND. kPsi(i,j,bi,bj) ) THEN
#else
             IF ( d2Loc.EQ.d2Min ) THEN
#endif
               xyLoc = xy0 + i + (j-1)*xLine
               IF ( xyMin.EQ.0. _d 0 ) THEN
                 xyMin = xyLoc
               ELSE
                 xyMin = MIN( xyMin, xyLoc )
               ENDIF
               iPsi0(bi,bj) = i
               jPsi0(bi,bj) = j
             ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        xyLoc = (sNx+1)*(sNy+1)*nSx*nSy*nPx*nPy + 2.
        IF ( xyMin.EQ.0. _d 0 ) xyMin = xyLoc
        xyMin = -xyMin
        _GLOBAL_MAX_RL( xyMin, myThid )
        xyMin = -xyMin
C-      select only one (based on minimum global-map index)
        _BARRIER
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_SET_CALC: ',
     &    'setting indices iPsi0,jPsi0 where Psi == 0 :'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1P1E19.6,A,0PF16.3)')
     &    'DIAGNOSTICS_SET_CALC: d2Min=',d2Min, ', ijMin=',xyMin
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        IF ( xyMin.EQ.xyLoc ) THEN
          WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_SET_CALC: ',
     &      'Fail to find the minimum distance => use Default'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          DO bj=1,nSy
           DO bi=1,nSx
             iPsi0(bi,bj) = -1
             jPsi0(bi,bj) =  0
           ENDDO
          ENDDO
        ELSE
          DO bj=1,nSy
           DO bi=1,nSx
             IF ( iPsi0(bi,bj).GT.0 ) THEN
              biG = bi-1+(myXGlobalLo-1)/sNx
              bjG = bj-1+(myYGlobalLo-1)/sNy
              xy0 = biG*(sNx+1) + bjG*(sNy+1)*xLine
              xyLoc = xy0 + iPsi0(bi,bj) + (jPsi0(bi,bj)-1)*xLine
              d2Loc = ABS( xyLoc - xyMin )
              IF ( d2Loc.GE.0.5 _d 0 ) THEN
               WRITE(msgBuf,'(2(A,2I5),A,F16.3)')
     &          ' discard: bi,bj=',bi,bj,
     &          ' ; i,j=',iPsi0(bi,bj),jPsi0(bi,bj),' ; ijLoc=',xyLoc
               CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                             SQUEEZE_RIGHT, myThid )
               iPsi0(bi,bj) = 0
               jPsi0(bi,bj) = 0
              ELSE
               WRITE(msgBuf,'(2(A,2I5),A,F16.3)')
     &          ' SELECT : bi,bj=',bi,bj,
     &          ' ; i,j=',iPsi0(bi,bj),jPsi0(bi,bj),' ; ijLoc=',xyLoc
               CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                             SQUEEZE_RIGHT, myThid )
              ENDIF
             ENDIF
c            WRITE(standardMessageUnit,'(2(A,2I5))')
c    &        ' bi,bj=',bi,bj,' i,jPsi0=', iPsi0(bi,bj),jPsi0(bi,bj)
           ENDDO
          ENDDO
        ENDIF
        WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
        _BARRIER
      ENDIF

      RETURN
      END
