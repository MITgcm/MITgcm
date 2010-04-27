C $Header: /u/gcmpack/MITgcm/pkg/autodiff/global_sum_ad.F,v 1.11 2010/04/27 19:27:54 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C--   File global_adsum.F: Routines that perform adjoint of
C                          global sum on all threads and all processors
C      Contents
C      o GLOBAL_ADSUM_R4
C      o GLOBAL_ADSUM_R8

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_ADSUM_R4

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE GLOBAL_ADSUM_R4(
     I                            myThid,
     U                            adsumPhi
     &                          )
#else
      SUBROUTINE GLOBAL_ADSUM_R4(
     U                            adsumPhi,
     I                            myThid
     &                          )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_ADSUM_R4
C     | o Handle sum for real*4 data.
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      Real*4 adsumPhi
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

      CALL GLOBAL_SUM_R4( adsumPhi, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_ADSUM_R8

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE GLOBAL_ADSUM_R8(
     I                            myThid,
     U                            adsumPhi
     &                          )
#else
      SUBROUTINE GLOBAL_ADSUM_R8(
     U                            adsumPhi,
     I                            myThid
     &                          )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_ADSUM_R8
C     | o Handle sum for real*8 data.
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      Real*8 adsumPhi
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==

      CALL GLOBAL_SUM_R8( adsumPhi, myThid )

      RETURN
      END
