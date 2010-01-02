C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/dic_aver_final.F,v 1.10 2010/01/02 23:07:39 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE DIC_AVER_FINAL(
     I           myThid)

C     *==========================================================*
C     | SUBROUTINE DIC_AVER_FINAL
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#ifdef DIC_BIOTIC
#include "DIC_DIAGS.h"
#include "DIC_COST.h"
#endif

C     == Routine arguments ==
      INTEGER myThid

#ifdef ALLOW_DIC_COST
#ifdef ALLOW_TIMEAVE

C     == Local variables ==
      INTEGER bi, bj

C     Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZE(PO4ann, OBS_timetave, Nr ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(O2ann, OBS_timetave, Nr ,
     &                 bi,bj,myThid)
Cswdmonth-add
         CALL TIMEAVE_NORMALIZ(PO4lev1, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PO4lev2, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PO4lev3, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
cQQ      CALL TIMEAVE_NORMALIZ(PO4lev4, OBSM_timetave, 12 ,
cQQ  &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2lev1, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2lev2, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(O2lev3, OBSM_timetave, 12 ,
     &                 bi,bj,myThid)
cQQ      CALL TIMEAVE_NORMALIZ(O2lev4, OBSM_timetave, 12 ,
cQQ  &                 bi,bj,myThid)

        ENDDO
       ENDDO

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_DIC_COST */

      RETURN
      END
