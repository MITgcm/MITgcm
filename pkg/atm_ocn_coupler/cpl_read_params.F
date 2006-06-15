C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_read_params.F,v 1.1 2006/06/15 23:05:25 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_READ_PARAMS
C     *==========================================================*
C     | SUBROUTINE CPL_READ_PARAMS
C     | o Read coupler and mapping parameters 
C     *==========================================================*
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "CPL_PARAMS.h"
#include "CPL_MAP2GRIDS.h"

C     == Routine arguments ==
CEndOfInterface

C     == Local variables ==
        
        OPEN(88, FILE='data', STATUS='OLD')
        READ(88,*) nCouplingSteps
        READ(88,*) nROmap
        READ(88,'(A)') runoffmapFile
        CLOSE(88)

        WRITE(6,*) 'CPL_READ_PARAMS: nCouplingSteps=',nCouplingSteps

      RETURN
      END
