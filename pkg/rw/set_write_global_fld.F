C $Header: /u/gcmpack/MITgcm/pkg/rw/set_write_global_fld.F,v 1.2 2004/11/11 01:25:05 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE SET_WRITE_GLOBAL_FLD ( flag )
      IMPLICIT NONE
C SET_WRITE_GLOBAL_FLD( flag ) sets an internal logical state to
C indicate whether files written by subsequent call to the
C READ_WRITE_FLD package should create "global" or "tiled" files.
C   flag = .TRUE.  indicates "global" files
C   flag = .FALSE. indicates "tiled" files
C
C Arguments (input)
      LOGICAL flag
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C
      globalFile=flag
C
      RETURN
      END
