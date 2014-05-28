C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_readparms.F,v 1.17 2014/05/28 00:16:28 jmc Exp $
C $Name:  $

#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE GRDCHK_READPARMS( mythid )

c     ==================================================================
c     SUBROUTINE grdchk_readparms
c     ==================================================================
c
c     o Initialize the ECCO gradient check.
c
c     started: Christian Eckert eckert@mit.edu 03-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "ctrl.h"
#include "grdchk.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      INTEGER iGloTile, jGloTile
c     integer i,j,k
c     integer bi,bj
c     integer itlo,ithi
c     integer jtlo,jthi
c     integer jmin,jmax
c     integer imin,imax

      integer errio
      integer il
      integer iUnit

      character*(max_len_mbuf) msgbuf
      character*(max_len_prec) record

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Optimization parameters.
      namelist /grdchk_nml/
     &                  grdchk_eps,
     &                  nbeg,
     &                  nstep,
     &                  nend,
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

c     jtlo = mybylo(mythid)
c     jthi = mybyhi(mythid)
c     itlo = mybxlo(mythid)
c     ithi = mybxhi(mythid)
c     jmin = 1-oly
c     jmax = sny+oly
c     imin = 1-olx
c     imax = snx+olx

      _BEGIN_MASTER( mythid )

c--     Set default values.
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

c       Next, read the calendar data file.
        WRITE(msgBuf,'(A)') 'GRDCHK_READPARMS: opening data.grdchk'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.grdchk', 'GRDCHK_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = grdchk_nml)

        WRITE(msgBuf,'(A)')
     &     'GRDCHK_READPARMS: finished reading data.grdchk'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                SQUEEZE_RIGHT , 1)

        CLOSE( iUnit )

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

c--     Summarize the gradient check setup.
        call grdchk_Summary( mythid )

      _END_MASTER( mythid )
      _BARRIER

#endif /* ALLOW_GRDCHK */

      RETURN
      END
