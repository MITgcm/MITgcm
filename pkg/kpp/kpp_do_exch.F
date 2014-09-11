C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_do_exch.F,v 1.4 2014/09/11 19:22:24 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

CBOP
C     !ROUTINE: KPP_DO_EXCH
C     !INTERFACE:
      SUBROUTINE KPP_DO_EXCH( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_DO_EXCH
C     | o fill overlap regions of KPP arrays by calling exchanges
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
c#include "KPP_PARAMS.h"
#include "KPP.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid    :: My Thread Id number
      INTEGER myThid

#ifdef ALLOW_KPP
C     !LOCAL VARIABLES:
CEOP

#ifndef ALLOW_AUTODIFF
      CALL EXCH_3D_RL( KPPviscAz, Nr, myThid )
#else
      _EXCH_XYZ_RL( KPPviscAz, myThid )
#endif

#endif /* ALLOW_KPP */

      RETURN
      END
