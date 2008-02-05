C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_set_levels.F,v 1.13 2008/02/05 15:31:19 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP 0
C     !ROUTINE: DIAGNOSTICS_SET_LEVELS

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SET_LEVELS( myThid )

C     !DESCRIPTION:
C     Initialize Diagnostic Levels, according to GDIAG
C      for all available diagnostics
C     Notes: needs to be called after all packages set they own available
C            diagnostics

C     \begin{center}
C       \begin{tabular}[h]{|c|c|}\hline
C         \textbf{Positions}  &  \textbf{Characters}
C         &  \textbf{Meanings} \\\hline
C         parse(10) &  0  &  levels = 0  \\
C                   &  1  &  levels = 1  \\
C                   &  R  &  levels = Nr  \\
C                   &  L  &  levels = MAX(Nr,NrPhys)  \\
C                   &  M  &  levels = MAX(Nr,NrPhys) - 1  \\
C                   &  G  &  levels = Ground_level Number \\
C                   &  I  &  levels = sea-Ice_level Number \\
C       \end{tabular}
C     \end{center}

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#define SIZE_IS_SET
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
       INTEGER Nrphys
       PARAMETER (Nrphys=0)
#endif

#ifdef ALLOW_LAND
#include "LAND_SIZE.h"
#else
      INTEGER land_nLev
      PARAMETER ( land_nLev = 0 )
#endif

C     !INPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER l, n, ncount
      INTEGER nlevs, nGroundLev
      INTEGER  dUnit, stdUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*84 ccHead, ccLine
      CHARACTER*8  blk8c
      CHARACTER*10 gcode
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER( myThid )

      nlevs   = MAX(Nr,Nrphys)
      nGroundLev = land_nLev

C     Diagnostic Levels
C     -----------------
      DO n = 1,ndiagt
        IF     (gdiag(n)(10:10) .EQ. '0') THEN
          kdiag(n) = 0
        ELSEIF (gdiag(n)(10:10) .EQ. '1') THEN
          kdiag(n) = 1
        ELSEIF (gdiag(n)(10:10) .EQ. 'R') THEN
          kdiag(n) = Nr
        ELSEIF (gdiag(n)(10:10) .EQ. 'L') THEN
          kdiag(n) = nlevs
        ELSEIF (gdiag(n)(10:10) .EQ. 'M') THEN
          kdiag(n) = nlevs - 1
        ELSEIF (gdiag(n)(10:10) .EQ. 'G') THEN
          kdiag(n) = nGroundLev
        ELSEIF (gdiag(n)(10:10) .NE. ' ') THEN
C-      others: set 1 level:
          kdiag(n) = 1
        ENDIF
      ENDDO

C--   Check for unvalid diag.mate number
      ncount = 0
      DO n = 1,ndiagt
       IF ( hdiag(n).LT.0 .OR. hdiag(n).GT.ndiagt ) THEN
        WRITE(msgBuf,'(2A,I6,3A)') 'DIAGNOSTICS_SET_LEVELS: ',
     &    'unvalid mate number=',hdiag(n),' for diag."',cdiag(n),'"'
        CALL PRINT_ERROR( msgBuf , myThid )
        ncount = ncount + 1
       ENDIF
      ENDDO
      IF ( ncount.GT.0 ) THEN
        WRITE(msgBuf,'(2A,I6,3A)') 'DIAGNOSTICS_SET_LEVELS: ',
     &    'found', ncount, ' unvalid mate number(s) => STOP'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DIAGSTATS_SET_LEVELS'
      ENDIF

      stdUnit = standardMessageUnit
      WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)') 'DIAGNOSTICS_SET_LEVELS: done'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A,I6)')
     &   ' Total Nb of available Diagnostics: ndiagt=', ndiagt
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     write a summary of the (long) list of all available diagnostics:
      IF ( debugLevel.GE.debLevA ) THEN

        WRITE(msgBuf,'(2A)')
     &   ' write list of available Diagnostics to file: ',
     &   'available_diagnostics.log'
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)

        WRITE(ccHead,'(2A)')
     &   '  Num  |<-Name->|Levs|  mate |<- code ->|',
     &   '<--  Units   -->|<- Tile (max=80c)'
        DO l=1,LEN(ccLine)
         ccLine(l:l) = '-'
        ENDDO

        CALL MDSFINDUNIT( dUnit, mythid )
        OPEN(dUnit, file='available_diagnostics.log',
     &              status='unknown', form='formatted')
        WRITE(dUnit,'(A,I6)')
     &   ' Total Nb of available Diagnostics: ndiagt=', ndiagt
        WRITE(dUnit,'(A)') ccLine
        WRITE(dUnit,'(A)') ccHead
        WRITE(dUnit,'(A)') ccLine
        DO n=1,ndiagt
         IF ( MOD(n,100).EQ.0 ) THEN
           WRITE(dUnit,'(A)') ccLine
           WRITE(dUnit,'(A)') ccHead
           WRITE(dUnit,'(A)') ccLine
         ENDIF
         l = ILNBLNK(tdiag(n))
         gcode = gdiag(n)(1:10)
         IF ( hdiag(n).NE.0 .AND. l.GE.1 ) THEN
           WRITE(dUnit,'(I6,3A,I3,A,I6,6A)') n, ' |', cdiag(n), '|',
     &                 kdiag(n),' |', hdiag(n), ' |', gcode, '|',
     &                 udiag(n), '|', tdiag(n)(1:l)
         ELSEIF ( hdiag(n).NE.0 ) THEN
           WRITE(dUnit,'(I6,3A,I3,A,I6,6A)') n, ' |', cdiag(n), '|',
     &                 kdiag(n),' |', hdiag(n), ' |', gcode, '|',
     &                 udiag(n), '|'
         ELSEIF (l.GE.1) THEN
           WRITE(dUnit,'(I6,3A,I3,6A)')      n, ' |', cdiag(n), '|',
     &                 kdiag(n),' |       |',         gcode, '|',
     &                 udiag(n), '|', tdiag(n)(1:l)
         ELSE
           WRITE(dUnit,'(I6,3A,I3,6A)')      n, ' |', cdiag(n), '|',
     &                 kdiag(n),' |       |',         gcode, '|',
     &                 udiag(n), '|'
         ENDIF
        ENDDO
        WRITE(dUnit,'(A)') ccLine
        WRITE(dUnit,'(A)') ccHead
        WRITE(dUnit,'(A)') ccLine
        CLOSE(dUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

C--   Check for multiple definition of the same diagnostic name
      blk8c  = '        '
      DO n = 2,ndiagt
       IF ( cdiag(n).NE.blk8c ) THEN
        DO l = 1,n-1
         IF ( cdiag(l).EQ.cdiag(n) ) THEN
            WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_SET_LEVELS: ',
     &       'diag.Name: ',cdiag(n),' registered 2 times :'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_SET_LEVELS: ',
     &       '1rst (l=', l, ' ), title= ',tdiag(l)
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_SET_LEVELS: ',
     &       ' 2nd (n=', n, ' ), title= ',tdiag(n)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_LEVELS'
         ENDIF
        ENDDO
       ENDIF
      ENDDO

C--   Check that number of levels to write (in data.diagnostics) does not
C     exceeds max size: nlevs=max(Nr,NrPhys)
C   note: max size of array to write has been changed to "numLevels",
C         so that this checking is no longer usefull since nlevels
C         cannot be larger than "numLevels" anyway.

      _END_MASTER( myThid )

C--   Everyone else must wait for the levels to be set
      _BARRIER

      RETURN
      END
