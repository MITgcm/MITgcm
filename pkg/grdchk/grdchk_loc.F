#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GRDCHK_LOC

C     !INTERFACE:
      SUBROUTINE GRDCHK_LOC(
     I                      icomp,
     I                      ichknum,
     O                      icvrec,
     O                      itile,
     O                      jtile,
     O                      layer,
     O                      obcspos,
     O                      itilepos,
     O                      jtilepos,
     O                      icglom1,
     O                      itest,
     O                      ierr,
     I                      myThid )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE GRDCHK_LOC
C     ==================================================================
C     o Get the location of a given component of the control vector for
C       the current process.
C     started: Christian Eckert eckert@mit.edu 04-Apr-2000
C     continued: heimbach@mit.edu: 13-Jun-2001
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "CTRL.h"
#ifdef ALLOW_OBCS_CONTROL
C     CTRL_OBCS.h must be included before GRDCHK.h
# include "CTRL_OBCS.h"
#endif
#include "GRDCHK.h"
#ifdef ALLOW_OBCS
# include "OBCS_GRID.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER icomp
      INTEGER ichknum
      INTEGER icvrec
      INTEGER jtile
      INTEGER itile
      INTEGER layer
      INTEGER obcspos
      INTEGER itilepos
      INTEGER jtilepos
      INTEGER itest
      INTEGER ierr
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER itmp,jtmp
      INTEGER iobcs
c     INTEGER biwrk,bjwrk
      INTEGER iwrk, jwrk, kwrk
c     INTEGER iobcswrk
      INTEGER irec, irecwrk
      INTEGER icglo, icglom1
      INTEGER icomptest
      INTEGER icomploc
      INTEGER nobcsmax
CEOP

      _BEGIN_MASTER( myThid )

C     initialise parameters
      ierr    = -5
      icglom1 = 0
      icomploc= 0

cph(
      print *, 'ph-test icomp, ncvarcomp, ichknum ',
     &     icomp, ncvarcomp, ichknum
cph)
      IF ( icomp .GT. 0 ) THEN
       IF ( icomp .LE. ncvarcomp ) THEN
C--     A valid component of the control variable has been selected.
        IF ( ichknum .EQ. 1 ) THEN
          itest     = 0
          icomptest = 0
          irecwrk   = 1
c         bjwrk     = 1
c         biwrk     = 1
          kwrk      = 1
c         iobcswrk  = 1
          jwrk      = 1
          iwrk      = 1
          icglo     = 0
        ELSE
          itest     = itestmem (ichknum-1)
          icomptest = icompmem (ichknum-1)
          irecwrk   = irecmem  (ichknum-1)
c         bjwrk     = bjmem    (ichknum-1)
c         biwrk     = bimem    (ichknum-1)
          kwrk      = klocmem  (ichknum-1)
c         iobcswrk  = iobcsmem (ichknum-1)
          icglo     = icglomem (ichknum-1)
          jwrk      = jlocmem  (ichknum-1)
          iwrk      = ilocmem  (ichknum-1)
          iwrk      = iwrk + 1
        ENDIF

C--   set max loop index for obcs multiplicities
        IF ( ncvargrd(grdchkvarindex) .EQ. 'm' ) THEN
#ifdef ALLOW_OBCS_CONTROL
         nobcsmax = nobcs
#else
         print *, 'S/R grdchk_loc: Ooops!'
#endif
        ELSE
          nobcsmax = 1
        ENDIF

cph(
cph-print        print *, 'ph-grd _loc: icomp, ichknum ',
cph-print     &       icomp, ichknum, ncvarcomp
cpj)
C--   Start to loop over records.
        DO irec = irecwrk, ncvarrecs(grdchkvarindex)
c        do iobcs = iobcswrk, nobcsmax
         iobcs = MOD((irec-1),nobcsmax) + 1
c        do bj = bjwrk, nSy
c         do bi = biwrk, nSx
           bj = jLocTile
           bi = iLocTile

           DO k = kwrk, ncvarnrmax(grdchkvarindex)
             icglo   = icglo + nwettile(bi,bj,k,iobcs)
             icglom1 = icglo - nwettile(bi,bj,k,iobcs)
cph(
        print *, 'ph-grd _loc: bi, bj, icomptest, ichknum ',
     &       bi, bj, icomptest, ichknum
cph-print        print *, 'ph-grd _loc: icglo ',
cph-print     &       k, icglo, icglom1, iwetsum(bi,bj,k)
cpj)
             IF ( (ierr .NE. 0) .AND.
     &            (icomp .GT. icglom1 .AND. icomp .LE. icglo) ) THEN
cph
cph             if ( (ierr .NE. 0) .AND.
cph     &            (icomp .GT.
cph     &              (iobcs-1)*iwetsum(bi,bj,nr)+iwetsum(bi,bj,k-1))
cph     &              .AND.
cph     &            (icomp .LE.
cph     &              (iobcs-1)*iwetsum(bi,bj,nr)+iwetsum(bi,bj,k))) then
cph
               IF ( icomptest .EQ. 0 ) THEN
                  icomptest = icglom1
               ENDIF
               icomploc = icomp
               icvrec = irec
               itile  = bi
               jtile  = bj
cph(
cph-print               print *, 'ph-grd irec, bj, bi, k ', irec, bj, bi, k
cpj)
               DO j = jwrk, ncvarymax(grdchkvarindex)
                DO i = iwrk, ncvarxmax(grdchkvarindex)
                 IF (ierr .NE. 0) THEN
                  IF ( ncvargrd(grdchkvarindex) .EQ. 'c' ) THEN
                     IF ( maskC(i,j,k,bi,bj) .GT. 0.) THEN
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     ENDIF
                  ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 's' ) THEN
                     IF ( _maskS(i,j,k,bi,bj) .GT. 0.) THEN
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     ENDIF
                  ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'w' ) THEN
                     IF ( _maskW(i,j,k,bi,bj) .GT. 0.) THEN
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     ENDIF
#ifdef ALLOW_SHELFICE
                  ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'i' ) THEN
                     IF ( maskSHI(i,j,k,bi,bj) .GT. 0.) THEN
                        icomptest = icomptest + 1
                        itmp = i
                        jtmp = j
                     ENDIF
#endif /* ALLOW_SHELFICE */
                  ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'm' ) THEN
                     IF ( grdchkvarindex .EQ. 11 ) THEN
#ifdef ALLOW_OBCSN_CONTROL
                        IF ( grdchk_maskxz(i,k,bi,bj,iobcs) .GT. 0.
     &                       .AND. j.EQ. OB_Jn(i,bi,bj) ) THEN
                           icomptest = icomptest + 1
                           itmp = i
                           jtmp = OB_Jn(i,bi,bj)
                        ENDIF
#endif
                     ELSEIF ( grdchkvarindex .EQ. 12 ) THEN
#ifdef ALLOW_OBCSS_CONTROL
                        IF ( grdchk_maskxz(i,k,bi,bj,iobcs) .GT. 0.
     &                       .AND. j.EQ. OB_Js(i,bi,bj) ) THEN
                           icomptest = icomptest + 1
                           itmp = i
                           jtmp = OB_Js(i,bi,bj)
                        ENDIF
#endif
                     ELSEIF ( grdchkvarindex .EQ. 13 ) THEN
#ifdef ALLOW_OBCSW_CONTROL
                        IF ( grdchk_maskyz(j,k,bi,bj,iobcs) .GT. 0.
     &                       .AND. i.EQ. OB_Iw(j,bi,bj) ) THEN
                           icomptest = icomptest + 1
                           itmp = OB_Iw(j,bi,bj)
                           jtmp = j
                        ENDIF
#endif
                     ELSEIF ( grdchkvarindex .EQ. 14 ) THEN
#ifdef ALLOW_OBCSE_CONTROL
                        IF ( grdchk_maskyz(j,k,bi,bj,iobcs) .GT. 0.
     &                       .AND. i.EQ. OB_Ie(j,bi,bj) ) THEN
                           icomptest = icomptest + 1
                           itmp = OB_Ie(j,bi,bj)
                           jtmp = j
                        ENDIF
#endif
                     ENDIF
                  ENDIF
cph(
cph-print                  print *, 'ph-grd icomp, icomptest, icomploc, i, j ',
cph-print     &                 icomp, icomptest, icomploc, i, j
cpj)
                  IF ( icomploc .EQ. icomptest ) THEN
                     itilepos = itmp
                     jtilepos = jtmp
                     layer    = k
                     obcspos  = iobcs
                     ierr     = 0
cph                     itest    = iwetsum(bi,bj,k)
cph(
                     print *, 'ph-grd -->hit<-- ', itmp,jtmp,k,iobcs
                     goto 1234
cph)
                  ENDIF
                 ENDIF
                ENDDO
                iwrk = 1
               ENDDO
               jwrk = 1
             ELSEIF (ierr .NE. 0) THEN
               IF (icomptest .EQ. icomp-1) THEN
                 icomptest = icomptest
               ELSE
                 icomptest = icomptest + nwettile(bi,bj,k,iobcs)
               ENDIF
cph(
cph-print               print *, 'ph-grd after loop icomptest, icomploc, k ',
cph-print     &              icomptest, icomploc, k
cph)
               iwrk      = 1
               jwrk      = 1
             ENDIF

C--   End of loop over k
           ENDDO
           kwrk = 1
C--   End of loop over bi
c         enddo
c         biwrk = 1
C--   End of loop over bj
c        enddo
c        bjwrk = 1
C--   End of loop over iobcs
c        enddo
c        iobcswrk = 1
C--   End of loop over irec records.
        ENDDO

       ELSE
C--   else icomp > ncvarcomp
         IF ( icomp .GT. maxncvarcomps ) THEN
C--        Such a component does not exist.
           ierr     = -4
           icvrec   = -1
           jtile    = -1
           itile    = -1
           layer    = -1
           obcspos  = -1
           jtilepos = -1
           itilepos = -1
         ELSE
C--        The component is a land point.
           ierr     = -3
           icvrec   = -1
           jtile    = -1
           itile    = -1
           layer    = -1
           obcspos  = -1
           jtilepos = -1
           itilepos = -1
         ENDIF
C--   End if/else block icomp =< ncvarcomp
       ENDIF
      ELSE
C--   else not( icomp > 0 )
         IF ( icomp .LT. 0 ) THEN
C--         Such a component does not exist.
            ierr     = -2
            icvrec   = -1
            jtile    = -1
            itile    = -1
            layer    = -1
            obcspos  = -1
            jtilepos = -1
            itilepos = -1
         ELSE
C--         Component zero.
            ierr     = -1
            icvrec   = -1
            jtile    = -1
            itile    = -1
            layer    = -1
            obcspos  = -1
            jtilepos = -1
            itilepos = -1
         ENDIF
C--   End if/else block icomp > 0
      ENDIF

 1234 CONTINUE

      _END_MASTER( myThid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      RETURN
      END
