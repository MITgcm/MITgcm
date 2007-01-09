C $Header: /u/gcmpack/MITgcm/model/src/Attic/set_write_global_pickup.F,v 1.1 2007/01/09 18:09:27 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SET_WRITE_GLOBAL_PICKUP
C     !INTERFACE:
      SUBROUTINE SET_WRITE_GLOBAL_PICKUP( flag )

C     !DESCRIPTION:
C     Sets an internal logical state to indicate whether files written
C     by subsequent calls to the READ_WRITE_FLD package should create
C     "global" or "tiled" files:
C     \begin{center}
C       \begin{tabular}[h]{|l|l|}\hline
C         \texttt{flag}  &  Meaning  \\\hline
C         \texttt{.TRUE.}  &  use ``global'' files  \\
C         \texttt{.TRUE.}  &  use ``tiled'' files  \\\hline
C       \end{tabular}
C     \end{center}

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      LOGICAL flag
CEOP
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

      globalFile = flag

      RETURN
      END
