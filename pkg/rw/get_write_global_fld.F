C $Header: /u/gcmpack/MITgcm/pkg/rw/get_write_global_fld.F,v 1.1 2004/11/11 01:25:05 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE GET_WRITE_GLOBAL_FLD ( flag )
      IMPLICIT NONE
C GET_WRITE_GLOBAL_FLD( flag ) gets the internal logical state
C that indicates whether to write a "global" or "tiled" files.
C READ_WRITE_FLD package should return 
C   flag = .TRUE.  indicates "global" files
C   flag = .FALSE. indicates "tiled" files
C
C Arguments (ouput)
      LOGICAL flag
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C
      flag=globalFile
C
      RETURN
      END
