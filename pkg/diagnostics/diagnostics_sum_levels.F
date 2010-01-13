C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_sum_levels.F,v 1.2 2010/01/13 01:20:38 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_SUM_LEVELS

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SUM_LEVELS(
     I                        listId, md, ndId, ip, im, lm,
     U                        fld3d,
     I                        undef,
     I                        myTime, myIter, myThid )

C     !DESCRIPTION:
C     Cumulate selected levels from a multi-level diagnostics field
C       before writing to file this level integrated output
C     (e.g., can be used to integrate vertically an Nr field).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

      INTEGER NrMax
      PARAMETER( NrMax = numLevels )

C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     md      :: field number in the list "listId".
C     ndId    :: diagnostics  Id number (in available diagnostics list)
C     ip      :: diagnostics  pointer to storage array
C     im      :: counter-mate pointer to storage array
C     lm      :: index in the averageCycle
C     fld3d   :: diagnostics field output array
C     undef   ::
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER listId, md, ndId, ip, im, lm
      _RL     fld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     undef
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
      INTEGER i, j, k
      INTEGER bi, bj
      INTEGER kLev
      _RL     tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpFac, hFacLoc
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(10) gcode

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (fflags(listId)(2:2).EQ.'I') THEN

        gcode = gdiag(ndId)(1:10)

C--   start loops on tile indices bi,bj:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              tmpFld(i,j) = 0. _d 0
            ENDDO
          ENDDO

          IF ( gcode(3:3).EQ.' ' ) THEN
C--   Cumulate levels directly:

             DO k = 1,nlevels(listId)
              kLev = NINT(levs(k,listId))
              DO j = 1-OLy,sNy+OLy
               DO i = 1-OLx,sNx+OLx
                tmpFld(i,j) = tmpFld(i,j) + fld3d(i,j,kLev,bi,bj)
               ENDDO
              ENDDO
             ENDDO
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
                fld3d(i,j,1,bi,bj) = tmpFld(i,j)
              ENDDO
             ENDDO

          ELSEIF ( gcode(3:3).EQ.'r' ) THEN
C--   Cumulate the level-thickness product:

             DO k = 1,nlevels(listId)
              kLev = NINT(levs(k,listId))
              IF ( gcode(9:9).EQ.'L' ) THEN
                tmpFac = drC(kLev)
              ELSE
                tmpFac = drF(kLev)
              ENDIF
              DO j = 1-OLy,sNy+OLy
               DO i = 1-OLx,sNx+OLx
                tmpFld(i,j) = tmpFld(i,j)
     &                      + tmpFac*fld3d(i,j,kLev,bi,bj)
               ENDDO
              ENDDO
             ENDDO
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
                fld3d(i,j,1,bi,bj) = tmpFld(i,j)
              ENDDO
             ENDDO

          ELSEIF ( gcode(3:3).EQ.'R' ) THEN
C--   Cumulate the level-thickness & hFac product:

             IF ( gcode(2:2).EQ.'M' ) THEN
               DO k = 1,nlevels(listId)
               kLev = NINT(levs(k,listId))
               IF ( gcode(9:9).EQ.'L' ) THEN
                 tmpFac = drC(kLev)
               ELSE
                 tmpFac = drF(kLev)
               ENDIF
               DO j = 1-OLy,sNy+OLy
                DO i = 1-OLx,sNx+OLx
                 tmpFld(i,j) = tmpFld(i,j)
     &                       + tmpFac*fld3d(i,j,kLev,bi,bj)
     &                               *hFacC(i,j,kLev,bi,bj)
                ENDDO
               ENDDO
              ENDDO
             ELSEIF ( gcode(2:2).EQ.'U' ) THEN
              DO k = 1,nlevels(listId)
               kLev = NINT(levs(k,listId))
               IF ( gcode(9:9).EQ.'L' ) THEN
                 tmpFac = drC(kLev)
               ELSE
                 tmpFac = drF(kLev)
               ENDIF
               DO j = 1-OLy,sNy+OLy
                DO i = 1-OLx,sNx+OLx
                 tmpFld(i,j) = tmpFld(i,j)
     &                       + tmpFac*fld3d(i,j,kLev,bi,bj)
     &                               *hFacW(i,j,kLev,bi,bj)
                ENDDO
               ENDDO
              ENDDO
             ELSEIF ( gcode(2:2).EQ.'V' ) THEN
              DO k = 1,nlevels(listId)
               kLev = NINT(levs(k,listId))
               IF ( gcode(9:9).EQ.'L' ) THEN
                 tmpFac = drC(kLev)
               ELSE
                 tmpFac = drF(kLev)
               ENDIF
               DO j = 1-OLy,sNy+OLy
                DO i = 1-OLx,sNx+OLx
                 tmpFld(i,j) = tmpFld(i,j)
     &                       + tmpFac*fld3d(i,j,kLev,bi,bj)
     &                               *hFacS(i,j,kLev,bi,bj)
                ENDDO
               ENDDO
              ENDDO
             ELSEIF ( gcode(2:2).EQ.'Z' ) THEN
              DO k = 1,nlevels(listId)
               kLev = NINT(levs(k,listId))
               IF ( gcode(9:9).EQ.'L' ) THEN
                 tmpFac = drC(kLev)
               ELSE
                 tmpFac = drF(kLev)
               ENDIF
               DO j = 2-OLy,sNy+OLy
                DO i = 2-OLx,sNx+OLx
                 hFacLoc = MIN(
     &                          hFacW( i, j, kLev,bi,bj),
     &                          hFacW( i,j-1,kLev,bi,bj),
     &                          hFacS( i, j, kLev,bi,bj),
     &                          hFacS(i-1,j, kLev,bi,bj)
     &                        )
                 tmpFld(i,j) = tmpFld(i,j)
     &                       + tmpFac*fld3d(i,j,kLev,bi,bj)
     &                               *hFacLoc
                ENDDO
               ENDDO
              ENDDO
             ELSE
               STOP 'DIAGNOSTICS_SUM_LEVELS: invalid gcode(2)'
             ENDIF
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
                fld3d(i,j,1,bi,bj) = tmpFld(i,j)
              ENDDO
             ENDDO

          ELSE
            STOP 'DIAGNOSTICS_SUM_LEVELS: Bad gcode(3) option'
          ENDIF

C-   end bi,bj loops
         ENDDO
        ENDDO

      ENDIF

      RETURN
      END
