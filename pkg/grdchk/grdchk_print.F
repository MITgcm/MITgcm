#include "GRDCHK_OPTIONS.h"
#include "AD_CONFIG.h"

      SUBROUTINE grdchk_print(
     I                         ichknum,
     I                         ierr_grdchk,
     I                         myThid )

C     ==================================================================
C     SUBROUTINE grdchk_print
C     ==================================================================
C
C     o Print the results of the gradient check.
C
C     started: Christian Eckert eckert@mit.edu 08-Mar-2000
C     continued: heimbach@mit.edu: 13-Jun-2001
C
C     ==================================================================
C     SUBROUTINE grdchk_print
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRDCHK.h"

C     == routine arguments ==
      INTEGER ichknum
      INTEGER ierr_grdchk
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      _RL fcref
      _RL fcpertplus, fcpertminus
      _RL xxmemo_ref
      _RL xxmemo_pert
      _RL gfd
      _RL adxxmemo
      _RL ftlxxmemo
      _RL ratio_ad
      _RL ratio_ftl
      _RL ratio_RMS
      INTEGER i
      INTEGER itile
      INTEGER jtile
      INTEGER itilepos
      INTEGER jtilepos
      INTEGER layer
      INTEGER icomp
      INTEGER ierr
      INTEGER numchecks
      INTEGER iL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      iL = ILNBLNK( grdchkvarname )

C--   Print header.
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// Gradient check results  >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c     WRITE(msgBuf,'(A,1PE14.6)')
c    & ' EPS = ',grdchk_eps
      WRITE(msgBuf,'(A,1PE13.6,3A)')
     & ' EPS =', grdchk_eps,
     & ' ; grdchk CTRL var/file name: "',grdchkvarname(1:iL),'"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(A,2X,4A,3(3X,A),11X,A)')
     & 'grdchk output h.p:', 'Id', ' Itile', ' Jtile',
     & ' LAYER', 'bi', 'bj', 'X(Id)', 'X(Id)+/-EPS'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A,2X,A,A4,1X,2A21)')
     &     'grdchk output h.c:', 'Id', 'FC', 'FC1', 'FC2'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#ifdef ALLOW_TANGENTLINEAR_RUN
      WRITE(msgBuf,'(A,2X,A,2X,2A18,4X,A18)')
     &     'grdchk output h.g:', 'Id',
     &     'FC1-FC2/(2*EPS)', 'TLM GRAD(FC)', '1-FDGRD/TLMGRD'
#else
      WRITE(msgBuf,'(A,2X,A,2X,2A18,4X,A18)')
     &     'grdchk output h.g:', 'Id',
     &     'FC1-FC2/(2*EPS)', 'ADJ GRAD(FC)', '1-FDGRD/ADGRD'
#endif
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--   Individual checks.
      IF ( ierr_grdchk .EQ. 0 ) THEN
         numchecks = ichknum
      ELSE
         numchecks = maxgrdchecks
      ENDIF

      ratio_RMS = 0.
      DO i = 1, numchecks
        xxmemo_ref   = xxmemref  (i)
        xxmemo_pert  = xxmempert (i)
        adxxmemo     = adxxmem   (i)
        ftlxxmemo    = ftlxxmem  (i)
        fcref        = fcrmem    (i)
        fcpertplus   = fcppmem   (i)
        fcpertminus  = fcpmmem   (i)
        gfd          = gfdmem    (i)
        ratio_ad     = ratioadmem(i)
        ratio_ftl    = ratioftlmem(i)
        itile        = bimem   (i)
        jtile        = bjmem   (i)
        itilepos     = ilocmem (i)
        jtilepos     = jlocmem (i)
        layer        = klocmem (i)
        icomp        = icompmem(i)
        ierr         = ierrmem (i)

        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I4,3I6,2I5,1x,1P2E17.9)')
     &       'grdchk output (p):',
     &       i, itilepos, jtilepos, layer, itile, jtile,
     &       xxmemo_ref, xxmemo_pert
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        IF ( ierr .EQ. 0 ) THEN
          WRITE(msgBuf,'(A,I4,1P3E21.13)')
     &          'grdchk output (c):',
     &          i, fcref, fcpertplus, fcpertminus
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
#ifdef ALLOW_TANGENTLINEAR_RUN
          ratio_RMS  = ratio_RMS + ratio_ftl*ratio_ftl
          WRITE(msgBuf,'(A,I4,3x,1P3E21.13)')
     &          'grdchk output (g):',
     &         i, gfd, ftlxxmemo, ratio_ftl
#else
          ratio_RMS  = ratio_RMS + ratio_ad*ratio_ad
          WRITE(msgBuf,'(A,I4,3x,1P3E21.13)')
     &          'grdchk output (g):',
     &          i, gfd, adxxmemo, ratio_ad
#endif
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ELSE
          IF ( ierr .EQ. -1 ) THEN
           WRITE(msgBuf,'(A)') ' Component does not exist (zero)'
          ELSEIF ( ierr .EQ. -2 ) THEN
           WRITE(msgBuf,'(A)') ' Component does not exist (negative)'
          ELSEIF ( ierr .EQ. -3 ) THEN
           WRITE(msgBuf,'(A)') ' Component does not exist (too large)'
          ELSEIF ( ierr .EQ. -4 ) THEN
           WRITE(msgBuf,'(A)') ' Component does not exist (land point)'
          ELSE
           WRITE(msgBuf,'(A,I6,A)') ' Unknown error (ierr=', ierr, ' )'
          ENDIF
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDDO

C--   Print final lines.
      IF ( ichknum.GT.1 ) ratio_RMS = ratio_RMS / ichknum
      IF ( ratio_RMS.GT.0. ) ratio_RMS = SQRT( ratio_RMS )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,I4,A,1P1E21.13)')
     &  'grdchk  summary  :  RMS of ',ichknum,' ratios =',ratio_RMS
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// Gradient check results  >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#endif /* ALLOW_GRDCHK */

      RETURN
      END
