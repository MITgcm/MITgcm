C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_readparms.F,v 1.12 2007/10/09 00:05:45 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"


      subroutine grdchk_readparms( mythid )

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

#include "EEPARAMS.h"
#include "SIZE.h"

#include "ctrl.h"
#include "grdchk.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

      integer i,j,k
      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

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

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      _BEGIN_MASTER( mythid )

c--     Set default values.
        grdchk_eps      = 1. _d 0
        nbeg            = 0
        nend            = 0
        nstep           = 0
        useCentralDiff  = .TRUE.
        grdchkwhichproc = 0
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
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , 1)
           STOP 'S/R grdchk_readparms'
        ENDIF
        IF ( jGloTile .GT. 1 .AND. useCubedSphereExchange ) THEN
           WRITE(msgBuf,'(A)')
     &          'jGloTile must be = 1 for useCubedSphereExchange'
           STOP 'S/R grdchk_readparms'
        ENDIF

c--     Summarize the gradient check's setup.
        call grdchk_Summary( mythid )
      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      end


