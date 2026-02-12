#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE GRDCHK_GET_MASK( myThid )

C     ==================================================================
C     SUBROUTINE grdchk_get_mask
C     ==================================================================
C
C     o Get the location of a given component of the control vector for
C       the current process.
C
C     started: Christian Eckert eckert@mit.edu 04-Apr-2000
C     continued: heimbach@mit.edu: 13-Jun-2001
C
C     ==================================================================
C     SUBROUTINE grdchk_get_mask
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
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
      INTEGER bi, bj
      INTEGER k, iobcs
      INTEGER itlo, ithi
      INTEGER jtlo, jthi
      INTEGER nobcsmax
C     == end of interface ==

      jtlo = 1
      jthi = nSy
      itlo = 1
      ithi = nSx

      _BEGIN_MASTER( myThid )

#ifdef ALLOW_OBCS_CONTROL
      nobcsmax = nobcs
#else
      nobcsmax = 1
#endif

C--   initialise
      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO k = 1,ncvarnrmax(grdchkvarindex)
         DO iobcs = 1, nobcsmax
           nwettile(bi,bj,k,iobcs) = 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Determine the number of components of the given
C--   control variable on the current tile.
      IF ( ncvargrd(grdchkvarindex) .EQ. 'c' ) THEN
        DO bj = jtlo,jthi
         DO bi = itlo,ithi
          DO k = 1,ncvarnrmax(grdchkvarindex)
            nwettile(bi,bj,k,1) = nwetctile(bi,bj,k)
          ENDDO
         ENDDO
        ENDDO
      ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 's' ) THEN
        DO bj = jtlo,jthi
         DO bi = itlo,ithi
          DO k = 1,ncvarnrmax(grdchkvarindex)
            nwettile(bi,bj,k,1) = nwetstile(bi,bj,k)
          ENDDO
         ENDDO
        ENDDO
      ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'w' ) THEN
        DO bj = jtlo,jthi
         DO bi = itlo,ithi
          DO k = 1,ncvarnrmax(grdchkvarindex)
            nwettile(bi,bj,k,1) = nwetwtile(bi,bj,k)
          ENDDO
         ENDDO
        ENDDO
c     ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'v' ) THEN
c       DO bj = jtlo,jthi
c        DO bi = itlo,ithi
c         DO k = 1,ncvarnrmax(grdchkvarindex)
c           nwettile(bi,bj,k,1) = nwetvtile(bi,bj,k)
c         ENDDO
c        ENDDO
c       ENDDO
#ifdef ALLOW_SHELFICE
      ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'i' ) THEN
        DO bj = jtlo,jthi
         DO bi = itlo,ithi
          DO k = 1,ncvarnrmax(grdchkvarindex)
            nwettile(bi,bj,k,1) = nwetitile(bi,bj,k)
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_SHELFICE */
#ifdef ALLOW_OBCS_CONTROL
      ELSEIF ( ncvargrd(grdchkvarindex) .EQ. 'm' ) THEN
       DO bj = jtlo,jthi
         DO bi = itlo,ithi
          DO k = 1,ncvarnrmax(grdchkvarindex)
           DO iobcs = 1, nobcsmax
             IF ( ncvarindex(grdchkvarindex).EQ.1 ) THEN
#ifdef ALLOW_OBCSN_CONTROL
               nwettile(bi,bj,k,iobcs) = nwetobcsn(bi,bj,k,iobcs)
#endif
             ELSEIF ( ncvarindex(grdchkvarindex).EQ.2 ) THEN
#ifdef ALLOW_OBCSS_CONTROL
               nwettile(bi,bj,k,iobcs) = nwetobcss(bi,bj,k,iobcs)
#endif
             ELSEIF ( ncvarindex(grdchkvarindex).EQ.3 ) THEN
#ifdef ALLOW_OBCSE_CONTROL
               nwettile(bi,bj,k,iobcs) = nwetobcse(bi,bj,k,iobcs)
#endif
             ELSEIF ( ncvarindex(grdchkvarindex).EQ.4 ) THEN
#ifdef ALLOW_OBCSW_CONTROL
               nwettile(bi,bj,k,iobcs) = nwetobcsw(bi,bj,k,iobcs)
#endif
             ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_OBCS_CONTROL */
      ELSE
Ce        --> wrong grid specification for the control variable.
      ENDIF

C--   get mask file for obcs
#ifdef ALLOW_OBCS_CONTROL
      CALL GRDCHK_GET_OBCS_MASK( myThid )
#endif

C     ----------------------------------------------------------------

C--   Determine the actual and the maximum possible number of
C--   components of the given control variable.
      ncvarcomp     = 0
      maxncvarcomps = 0
      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO k = 1,ncvarnrmax(grdchkvarindex)
         DO iobcs = 1, nobcsmax
           ncvarcomp     = ncvarcomp + nwettile(bi,bj,k,iobcs)
           maxncvarcomps = maxncvarcomps
     &      + ncvarxmax(grdchkvarindex)*ncvarymax(grdchkvarindex)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ncvarcomp     = ncvarcomp*ncvarrecs(grdchkvarindex)
      maxncvarcomps = maxncvarcomps*ncvarrecs(grdchkvarindex)

      DO bj = jtlo,jthi
       DO bi = itlo,ithi
         iwetsum(bi,bj,0)    = 0
         DO k = 1,ncvarnrmax(grdchkvarindex)
           iwetsum(bi,bj,k) = iwetsum(bi,bj,k-1) + nwettile(bi,bj,k,1)
         ENDDO
       ENDDO
      ENDDO

      _END_MASTER( myThid )

      _BARRIER

#endif /* ALLOW_GRDCHK */

      RETURN
      END
