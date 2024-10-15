#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GRDCHK_GET_POSITION
C     !INTERFACE:
      SUBROUTINE GRDCHK_GET_POSITION( mythid )

C     !DESCRIPTION:
C     o Get the location of a given component of the control vector for
C       the current process.
C
C     started: Christian Eckert eckert@mit.edu 04-Apr-2000
C     continued: heimbach@mit.edu: 13-Jun-2001

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif
#include "CTRL_SIZE.h"
#include "CTRL.h"
#ifdef ALLOW_OBCS_CONTROL
C     CTRL_OBCS.h must be included before GRDCHK.h
# include "CTRL_OBCS.h"
#endif
#include "GRDCHK.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C     !LOCAL VARIABLES:
      INTEGER icvrec
      INTEGER jtile
      INTEGER itile
      INTEGER layer
      INTEGER obcspos
      INTEGER itilepos
      INTEGER jtilepos
      INTEGER itest
      INTEGER ierr
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER iobcs
      INTEGER iwrk, jwrk, kwrk
      INTEGER irec, irecwrk
      INTEGER icomptest
      INTEGER nobcsmax
      INTEGER pastit
      _RL wetlocal
CEOP

C     local copies of COMMON block variable (GRDCHK.h)
      itile    = iLocTile
      jtile    = jLocTile
      itilepos = iGloPos
      jtilepos = jGloPos
      layer    = kGloPos
      obcspos  = obcsglo
      icvrec   = recglo

      _BEGIN_MASTER( myThid )

C--   determine proc. number from following assumptions <= done in
C     grdchk_readparms

      IF ( myProcId .EQ. grdchkwhichproc ) THEN

C     initialise parameters
       ierr      = -5
       pastit    = -1
       wetlocal  = 0

       itest     = 0
       icomptest = 0
       irecwrk   = 1
       kwrk      = 1
       jwrk      = 1
       iwrk      = 1

C--   set max loop index for obcs multiplicities
       IF ( ncvargrd(grdchkvarindex) .EQ. 'm' ) THEN
#ifdef ALLOW_OBCS_CONTROL
        nobcsmax = nobcs
#else
        PRINT *, 'S/R grdchk_get_position: Ooops!'
#endif
       ELSE
        nobcsmax = 1
       ENDIF

C--   Start to loop over records.
       DO irec = irecwrk, ncvarrecs(grdchkvarindex)
        iobcs = MOD((irec-1),nobcsmax) + 1
        bi = itile
        bj = jtile
        DO k = kwrk, ncvarnrmax(grdchkvarindex)
cph(
cph-print               PRINT *, 'ph-grd get_pos irec, bj, bi, k ',
cph-print     &              irec, bj, bi, k
cph)
         IF ( ierr .ne. 0 ) THEN
          DO j = jwrk, ncvarymax(grdchkvarindex)
           DO i = iwrk, ncvarxmax(grdchkvarindex)
            IF (ierr .NE. 0) THEN
             IF ( ncvargrd(grdchkvarindex) .EQ. 'c' ) THEN
              IF ( maskC(i,j,k,bi,bj) .GT. 0.) THEN
               icomptest = icomptest + 1
              ENDIF
              wetlocal = maskC(i,j,k,bi,bj)
             ELSE IF ( ncvargrd(grdchkvarindex) .EQ. 's' ) THEN
              IF ( _maskS(i,j,k,bi,bj) .GT. 0.) THEN
               icomptest = icomptest + 1
              ENDIF
              wetlocal = _maskS(i,j,k,bi,bj)
             ELSE IF ( ncvargrd(grdchkvarindex) .EQ. 'w' ) THEN
              IF ( _maskW(i,j,k,bi,bj) .GT. 0.) THEN
               icomptest = icomptest + 1
              ENDIF
              wetlocal = _maskW(i,j,k,bi,bj)
#ifdef ALLOW_SHELFICE
C--   Ice shelf mask.
             ELSE IF ( ncvargrd(grdchkvarindex) .EQ. 'i' ) THEN
              IF ( maskSHI(i,j,k,bi,bj) .GT. 0.) THEN
               icomptest = icomptest + 1
              ENDIF
              wetlocal = maskSHI(i,j,k,bi,bj)
#endif /* ALLOW_SHELFICE */
#ifdef ALLOW_OBCS_CONTROL
             ELSE IF ( ncvargrd(grdchkvarindex) .EQ. 'm' ) THEN
              IF ( ncvarfname(grdchkvarindex)
     &             .EQ. xx_obcsn_file ) THEN
#ifdef ALLOW_OBCSN_CONTROL
               IF (grdchk_maskxz(i,k,bi,bj,iobcs) .GT. 0.
     &              .AND. j .EQ. OB_Jn(I,bi,bj) ) THEN
                icomptest = icomptest + 1
               ENDIF
               wetlocal = grdchk_maskxz(i,k,bi,bj,iobcs)
#endif
              ELSE IF ( ncvarfname(grdchkvarindex)
     &                    .EQ. xx_obcss_file ) THEN
#ifdef ALLOW_OBCSS_CONTROL
               IF (grdchk_maskxz(i,k,bi,bj,iobcs) .GT. 0.
     &              .AND. j .EQ. OB_Js(I,bi,bj) ) THEN
                icomptest = icomptest + 1
               ENDIF
               wetlocal = grdchk_maskxz(i,k,bi,bj,iobcs)
#endif
              ELSE IF ( ncvarfname(grdchkvarindex)
     &              .EQ. xx_obcse_file ) THEN
#ifdef ALLOW_OBCSE_CONTROL
               IF (grdchk_maskyz(j,k,bi,bj,iobcs) .GT. 0.
     &              .AND. i .EQ. OB_Ie(J,bi,bj) ) THEN
                icomptest = icomptest + 1
               ENDIF
               wetlocal = grdchk_maskyz(j,k,bi,bj,iobcs)
#endif
              ELSE IF ( ncvarfname(grdchkvarindex)
     &              .EQ. xx_obcsw_file ) THEN
#ifdef ALLOW_OBCSW_CONTROL
               IF ( grdchk_maskyz(j,k,bi,bj,iobcs) .GT. 0.
     &              .AND. i .EQ. OB_Iw(J,bi,bj) ) THEN
                icomptest = icomptest + 1
               ENDIF
               wetlocal = grdchk_maskyz(j,k,bi,bj,iobcs)
#endif
              ENDIF
#endif /* ALLOW_OBCS_CONTROL */
             ENDIF

             IF ( i     .EQ. itilepos .AND.
     &            j     .EQ. jtilepos .AND.
     &            k     .EQ. layer .AND.
     &            bi    .EQ. itile .AND.
     &            bj    .EQ. jtile .AND.
     &            iobcs .EQ. obcspos .AND.
     &            irec  .EQ. icvrec ) THEN
              pastit = 0
              IF ( wetlocal .NE.0 ) THEN
               nbeg = icomptest
               nend = nbeg + nend
               ierr     = 0
               WRITE(standardMessageUnit,'(a,6I5)')
     &              ' grad-res exact position met: '
               WRITE(standardMessageUnit,'(a,7I5)')
     &              ' grad-res ', grdchkwhichproc,
     &              nbeg, itilepos, jtilepos, layer,
     &              itile, jtile
               GOTO 1234
              ENDIF
             ELSE IF ( pastit .EQ. 0 .AND. wetlocal .NE.0 ) THEN
              nbeg = icomptest
              nend = nbeg + nend
              ierr     = 0
              WRITE(standardMessageUnit,'(a,6I5)')
     &             ' grad-res closest next position: '
              WRITE(standardMessageUnit,'(a,7I5)')
     &             ' grad-res ', grdchkwhichproc,
     &             nbeg, itilepos, jtilepos, layer,
     &             itile, jtile
              GOTO 1234
             ENDIF

            ENDIF
           ENDDO
           iwrk = 1
          ENDDO
          jwrk = 1
         ELSE IF (ierr .NE. 0) THEN
          itest     = itest + nwettile(bi,bj,k,iobcs)
          iwrk      = 1
          jwrk      = 1
         ENDIF

C--   End of loop over k
        ENDDO

C--   End of loop over irec records.
       ENDDO

C--   End of if myProcId statement
      ENDIF

 1234 CONTINUE

      _END_MASTER( myThid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      RETURN
      END
