#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE GRDCHK_READPARMS( myThid )

C     ==================================================================
C     SUBROUTINE grdchk_readparms
C     ==================================================================
C
C     o Read Gradient-check package parameters
C
C     started: Christian Eckert eckert@mit.edu 03-Mar-2000
C     continued: heimbach@mit.edu: 13-Jun-2001
C
C     ==================================================================
C     SUBROUTINE grdchk_readparms
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "CTRL_SIZE.h"
#include "CTRL.h"
#ifdef ALLOW_OBCS_CONTROL
C     CTRL_OBCS.h must be included before GRDCHK.h
# include "CTRL_OBCS.h"
#endif
#include "GRDCHK.h"

C     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C     == local variables ==
      INTEGER iGloTile, jGloTile
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

C--   Gradient-Check parameters.
      NAMELIST /grdchk_nml/
     &                  grdchk_eps,
     &                  nbeg,
     &                  nstep,
     &                  nend,
     &                  grdchkvarname,
     &                  grdchkvarindex,
     &                  useCentralDiff,
     &                  grdchkwhichproc,
     &                  iGloPos,
     &                  jGloPos,
     &                  kGloPos,
     &                  iGloTile,
     &                  jGloTile,
     &                  idep,
     &                  jdep,
     &                  obcsglo,
     &                  recglo

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGrdChk ) THEN
C-    pkg GRDCHK is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.grdchk is found
         CALL PACKAGES_UNUSED_MSG( 'useGrdChk', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

C--     Set default values.
        grdchk_eps      = 1. _d 0
        nbeg            = 0
        nend            = 0
        nstep           = 0
        useCentralDiff  = .TRUE.
        grdchkwhichproc = -1
        iGloPos         = 0
        jGloPos         = 0
        kGloPos         = 1
        iGloTile        = 1
        jGloTile        = 1
        idep            = 1
        jdep            = 1
        obcsglo         = 1
        recglo          = 1
        grdchkvarname   = ' '
        grdchkvarindex  = UNSET_I

C       Next, read the GRDCHK parameter file.
        WRITE(msgBuf,'(A)') 'GRDCHK_READPARMS: opening data.grdchk'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.grdchk', 'GRDCHK_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = grdchk_nml)

        WRITE(msgBuf,'(A)')
     &     'GRDCHK_READPARMS: finished reading data.grdchk'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
        CLOSE(iUnit)
#else
        CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

        IF ( iGloPos .GT. sNx .OR. jGloPos .GT. sNy ) THEN
           WRITE(msgBuf,'(A)') 'i/j GloPos must be <= sNx/y'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R GRDCHK_READPARMS'
        ENDIF
        IF ( iGloTile .GT. nSx*nPx .OR. jGloTile .GT. nSy*nPy ) THEN
           WRITE(msgBuf,'(A)') 'i/j GloTile must be <= nSx*nPx/y'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R GRDCHK_READPARMS'
        ENDIF
        IF ( grdchkwhichproc .NE. -1 ) THEN
           WRITE(msgBuf,'(2A)') 'S/R GRDCHK_READPARMS: ',
     &         'grdchkwhichproc no longer allowed in namelist'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R GRDCHK_READPARMS'
        ENDIF

C--    From Tile Global-Indices, set Tile Local-Indices and proc. number
        iLocTile = iGloTile - (myXGlobalLo-1)/sNx
        jLocTile = jGloTile - (myYGlobalLo-1)/sNy
        IF ( iLocTile.GE.1 .AND. iLocTile.LE.nSx .AND.
     &       jLocTile.GE.1 .AND. jLocTile.LE.nSy ) THEN
          grdchkwhichproc = myProcId
        ENDIF

      _END_MASTER( myThid )
      _BARRIER

#endif /* ALLOW_GRDCHK */

      RETURN
      END
