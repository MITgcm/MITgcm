C $Header: /u/gcmpack/MITgcm/model/src/ini_model_io.F,v 1.20 2005/09/06 02:16:09 edhill Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MODEL_IO

C     !INTERFACE:
      SUBROUTINE INI_MODEL_IO( myThid )

C     !DESCRIPTION: 
C     Pass specific setup data for mdsio/rw.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP


C     Flags specific to RW and MDSIO
C     Set globalFiles flag for READ_WRITE_FLD package
      CALL SET_WRITE_GLOBAL_FLD( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_REC( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_PICKUP( globalFiles )


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
