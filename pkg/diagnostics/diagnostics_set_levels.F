C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_set_levels.F,v 1.1 2004/12/13 21:43:54 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP 0
C     !ROUTINE: DIAGNOSTICS_SET_LEVELS

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SET_LEVELS( myThid )

C     !DESCRIPTION:
C     Initialize Diagnostic Levels, according to GDIAG
C      for all available diagnostics
C     Notes: needs to be called after all packages set they own available
C            diagnostics

C     \begin{center}
C       \begin{tabular}[h]{|c|c|}\hline
C         \textbf{Positions}  &  \textbf{Characters}
C         &  \textbf{Meanings} \\\hline
C         parse(10) &  0  &  levels = 0  \\
C                   &  1  &  levels = 1  \\
C                   &  R  &  levels = Nr  \\
C                   &  L  &  levels = MAX(Nr,NrPhys)  \\
C                   &  M  &  levels = MAX(Nr,NrPhys) - 1  \\
C       \end{tabular}
C     \end{center}

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
c #include "EESUPPORT.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
       INTEGER Nrphys
       PARAMETER (Nrphys=0)
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      INTEGER n, nlevs

      nlevs   = MAX(Nr,Nrphys)

c     Diagnostic Levels
c     -----------------
      DO n = 1,ndiagt
        IF (gdiag(n)(10:10) .EQ. '0')  kdiag(n) = 0
        IF (gdiag(n)(10:10) .EQ. '1')  kdiag(n) = 1
        IF (gdiag(n)(10:10) .EQ. 'R')  kdiag(n) = Nr
        IF (gdiag(n)(10:10) .EQ. 'L')  kdiag(n) = nlevs
        IF (gdiag(n)(10:10) .EQ. 'M')  kdiag(n) = nlevs - 1
      ENDDO

      RETURN
      END
