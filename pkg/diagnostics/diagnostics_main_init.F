C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_main_init.F,v 1.2 2004/12/15 00:20:37 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_MAIN_INIT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_MAIN_INIT( myThid )

C     !DESCRIPTION:
C     Initialize available diagnostics list for variables of the main code
C     (not part of a package): set the following attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), and title (=tdiag)
C     Notes: 1) diagnostics defined here do not require any EQUIVALENCE
C            since they get filled-in with S/R FILL_DIAGNOSTICS
C            2) GDIAG is defined as character*16 and can be to character*1
C            parse(16) with the following codes currently defined:

C     \begin{center}
C       \begin{tabular}[h]{|c|c|}\hline
C         \textbf{Positions}  &  \textbf{Characters}
C         &  \textbf{Meanings} \\\hline
C         parse(1)  &  S  &  scalar \\
C                   &  U  &  U-vector component  \\
C                   &  V  &  V-vector component  \\
C         parse(2)  &  U  &  C-grid U-Point  \\
C                   &  V  &  C-grid V-Point  \\
C                   &  M  &  C-grid Mass Point  \\
C                   &  Z  &  C-grid Corner Point  \\
C         parse(3)  &  --  &  --Unused--  \\
C         parse(4)  &  P  &  positive definite  \\
C         parse(5 ) &  C  &  with counter array  \\
C                   &  D  &  disable an array for output  \\
C         parse(6--8) & '123'  &  3-digit mate number \\
C         parse(9)  &  U  &  model-level plus 1/2  \\
C                   &  M  &  model-level middle  \\
C                   &  L  &  model-level minus 1/2  \\
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

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     For each output variable,
C     specify Name (cdiag, 8c), Descriptions (tdiag, *c), Units (udiag, 16c) 
C         and Type/Parms (location on C grid, 2D/3D, ...) (gdiag, 16c)
C----------------------------------------------------------------------

      diagName  = 'PHIHYD  '
      diagTitle = 'Hydrostatic (ocean) pressure / (atmos) geo-Potential'
      diagUnits = 'm2/s2           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST(
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PHIBOT  '
      diagTitle = 'ocean bottom pressure / top. atmos geo-Potential'
      diagUnits = 'm2/s2           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST(
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PHIBOTSQ'
      diagTitle = 'Square of ocean bottom pressure / top. geo-Potential'
      diagUnits = 'm4/s4           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST(
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DRHODR  '
      diagTitle = 'Stratification: d.Sigma/dr (kg/m3/r_unit)       '
      diagUnits = 'kg/m4           '
      diagCode  = 'SM      LR      '
      CALL DIAGNOSTICS_ADD2LIST(
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DETADT2 '
      diagTitle = 'Square of Eta (Surf.P,SSH) Tendency ((r_unit/s)^2)'
      diagUnits = 'm2 or Pa2 /sec2 '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST(
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
