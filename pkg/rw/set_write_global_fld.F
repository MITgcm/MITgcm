C $Header: /u/gcmpack/MITgcm/pkg/rw/set_write_global_fld.F,v 1.1 2001/08/17 18:34:42 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE SET_WRITE_GLOBAL_FLD ( flag )
      IMPLICIT NONE
C SET_WRITE_GLOBAL_FLD( flag ) sets an internal logical state to
C indicate whether files written by subsequent call to the
C READ_WRITE_FLD package should create "global" or "tiled" files.
C   flag = .TRUE.  indicates "global" files
C   flag = .FALSE. indicates "tiled" files
C
C Arguments
      LOGICAL flag
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C
      globalFile=flag
C
      RETURN
      END
