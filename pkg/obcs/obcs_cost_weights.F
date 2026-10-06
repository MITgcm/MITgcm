#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE OBCS_COST_WEIGHTS( myThid )

c     ==================================================================
c     SUBROUTINE OBCS_COST_WEIGHTS
c     ==================================================================
c
c     o Read the weights used for the obcs cost function evaluation.
C       based on obsolete pkg/ecco/ecco_cost_weights.F
c
c     ==================================================================
c     SUBROUTINE OBCS_COST_WEIGHTS
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif

c     == routine arguments ==
      INTEGER  myThid

#ifdef ALLOW_OBCS_CONTROL
c     == external ==
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == local variables ==
      INTEGER k
      INTEGER gwUnit
      INTEGER ilo,ihi
      INTEGER iobcs
      _RL ratio
      _RL weights(Nr,4)
      _RL dummy
      logical  exst

c     == end of interface ==

c--   Initialize variance (weight) fields.
      DO iobcs = 1, nobcs
       DO k = 1, Nr
        weights(k,iobcs)= 0. _d 0
CML#ifdef ALLOW_OBCSN_CONTROL
CML        wobcsN(k,iobcs) = 0. _d 0
CML#endif
CML#ifdef ALLOW_OBCSS_CONTROL
CML        wobcsS(k,iobcs) = 0. _d 0
CML#endif
CML#ifdef ALLOW_OBCSE_CONTROL
CML        wobcsE(k,iobcs) = 0. _d 0
CML#endif
CML#ifdef ALLOW_OBCSW_CONTROL
CML        wobcsW(k,iobcs) = 0. _d 0
CML#endif
       ENDDO
      ENDDO

c--   Read error information and set up weight matrices.
      _BEGIN_MASTER(myThid)
      ilo = IFNBLNK(obcs_data_errfile)
      ihi = ILNBLNK(obcs_data_errfile)

      INQUIRE( file=obcs_data_errfile, exist=exst )
      IF (exst) THEN
        CALL OPEN_COPY_DATA_FILE(
     I                          obcs_data_errfile(ilo:ihi),
     I                          'OBCS_COST_WEIGHTS',
     O                          gwUnit,
     I                          myThid )

        READ(gwUnit,*) ratio, dummy
        DO k = 1, Nr
         READ(gwUnit,*) weights(k,1), weights(k,2), weights(k,3)
         weights(k,4) = weights(k,3)
        ENDDO
#ifdef SINGLE_DISK_IO
        CLOSE(gwUnit)
#else
        CLOSE(gwUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */
      ENDIF

      _END_MASTER(myThid)

      _BARRIER

      DO iobcs = 1,nobcs
       DO k = 1, Nr
# ifdef ALLOW_OBCSN_CONTROL
        wobcsN(k,iobcs) = weights(k,iobcs)
        IF (wobcsN(k,iobcs) .NE. 0.) THEN
         wobcsN(k,iobcs) =
     &        ratio/wobcsN(k,iobcs)/wobcsN(k,iobcs)
        ELSE
         wobcsN(k,iobcs) = 0.0 _d 0
        ENDIF
# endif
# ifdef ALLOW_OBCSS_CONTROL
        wobcsS(k,iobcs) = weights(k,iobcs)
        IF (wobcsS(k,iobcs) .NE. 0.) THEN
         wobcsS(k,iobcs) =
     &        ratio/wobcsS(k,iobcs)/wobcsS(k,iobcs)
        ELSE
         wobcsS(k,iobcs) = 0.0 _d 0
        ENDIF
# endif
# ifdef ALLOW_OBCSE_CONTROL
        wobcsE(k,iobcs) = weights(k,iobcs)
        IF (wobcsE(k,iobcs) .NE. 0.) THEN
         wobcsE(k,iobcs) =
     &        ratio/wobcsE(k,iobcs)/wobcsE(k,iobcs)
        ELSE
         wobcsE(k,iobcs) = 0.0 _d 0
        ENDIF
# endif
# ifdef ALLOW_OBCSW_CONTROL
        wobcsW(k,iobcs) = weights(k,iobcs)
        IF (wobcsW(k,iobcs) .NE. 0.) THEN
         wobcsW(k,iobcs) =
     &        ratio/wobcsW(k,iobcs)/wobcsW(k,iobcs)
        ELSE
         wobcsW(k,iobcs) = 0.0 _d 0
        ENDIF
# endif
       ENDDO
      ENDDO

#endif /* ALLOW_OBCS_CONTROL */

      RETURN
      END
