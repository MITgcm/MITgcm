C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_init_early.F,v 1.13 2008/02/05 15:13:01 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INIT_EARLY

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INIT_EARLY( myThid )

C     !DESCRIPTION:
C     Initialize available diagnostics list: set the following attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), and title (=tdiag)
C     Note: 1) diagnostics defined here are not presently filled. To use
C           one of them, one just needs to add a call to S/R DIAGNOSTICS_FILL
C           with the diagnostic name 'SDIAG...' or 'UDIAG...'
C           2) GDIAG is defined as character*16 and can be to character*1
C           parse(16) with the following codes currently defined:

C     \begin{center}
C       \begin{tabular}[h]{|c|c|}\hline
C         \textbf{Positions}  &  \textbf{Characters}
C         &  \textbf{Meanings} \\\hline
C         parse(1)  &  S  &  scalar \\
C                   &  U  &  vector component in X direction \\
C                   &  V  &  vector component in Y direction \\
C                   &  W  &  vector component in vertical direction \\
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
C                   &  G  &  levels = Ground_level Number \\
C                   &  I  &  levels = sea-Ice_level Number \\
C       \end{tabular}
C     \end{center}

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER n

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER( myThid )

      DO n=1,ndiagMax
        kdiag(n) = 0
        hdiag(n) = 0
        cdiag(n) = '        '
        gdiag(n) = '                '
        udiag(n) = '                '
        tdiag(n) = ' '
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     For each output variable,
C     specify Name (cdiag, 8c), Descriptions (tdiag, *c), Units (udiag, 16c)
C         and parsing code (location on C grid, 2D/3D, ...) (gdiag, 16c)
C----------------------------------------------------------------------
      cdiag( 1)  = 'SDIAG1  '
      tdiag( 1)  = 'User-Defined Surface   Diagnostic #1'
      udiag( 1)  = 'user-defined    '
      gdiag( 1)  = 'SM      L1      '

      cdiag( 2)  = 'SDIAG2  '
      tdiag( 2)  = 'User-Defined Surface   Diagnostic #2'
      udiag( 2)  = 'user-defined    '
      gdiag( 2)  = 'SM      L1      '

      cdiag( 3) = 'SDIAG3  '
      tdiag( 3) = 'User-Defined Surface   Diagnostic #3    '
      udiag( 3) = 'user-defined    '
      gdiag( 3) = 'SM      L1      '

      cdiag( 4) = 'SDIAG4  '
      tdiag( 4) = 'User-Defined Surface   Diagnostic #4    '
      udiag( 4) = 'user-defined    '
      gdiag( 4) = 'SM      L1      '

      cdiag( 5) = 'SDIAG5  '
      tdiag( 5) = 'User-Defined Surface   Diagnostic #5    '
      udiag( 5) = 'user-defined    '
      gdiag( 5) = 'SM      L1      '

      cdiag( 6) = 'SDIAG6  '
      tdiag( 6) = 'User-Defined Surface   Diagnostic #6    '
      udiag( 6) = 'user-defined    '
      gdiag( 6) = 'SM      L1      '

      cdiag( 7) = 'SDIAG7  '
      tdiag( 7) = 'User-Defined Surface   Diagnostic #7    '
      udiag( 7) = 'user-defined    '
      gdiag( 7) = 'SM      L1      '

      cdiag( 8) = 'SDIAG8  '
      tdiag( 8) = 'User-Defined Surface   Diagnostic #8    '
      udiag( 8) = 'user-defined    '
      gdiag( 8) = 'SM      L1      '

      cdiag( 9) = 'SDIAG9  '
      tdiag( 9) = 'User-Defined Surface   Diagnostic #9    '
      udiag( 9) = 'user-defined    '
      gdiag( 9) = 'SM      L1      '

      cdiag(10) = 'SDIAG10 '
      tdiag(10) = 'User-Defined Surface   Diagnostic #10   '
      udiag(10) = 'user-defined    '
      gdiag(10) = 'SM      L1      '

      cdiag(11)  = 'UDIAG1  '
      tdiag(11)  = 'User-Defined Upper-Air Diagnostic #1'
      udiag(11)  = 'user-defined    '
      gdiag(11)  = 'SM      ML      '

      cdiag(12)  = 'UDIAG2  '
      tdiag(12)  = 'User-Defined Upper-Air Diagnostic #2'
      udiag(12)  = 'user-defined    '
      gdiag(12)  = 'SM      ML      '

      cdiag(13) = 'UDIAG3  '
      tdiag(13) = 'User-Defined Upper-Air Diagnostic #3    '
      udiag(13) = 'user-defined    '
      gdiag(13) = 'SM      ML      '

      cdiag(14) = 'UDIAG4  '
      tdiag(14) = 'User-Defined Upper-Air Diagnostic #4    '
      udiag(14) = 'user-defined    '
      gdiag(14) = 'SM      ML      '

      cdiag(15) = 'UDIAG5  '
      tdiag(15) = 'User-Defined Upper-Air Diagnostic #5    '
      udiag(15) = 'user-defined    '
      gdiag(15) = 'SM      ML      '

      cdiag(16) = 'UDIAG6  '
      tdiag(16) = 'User-Defined Upper-Air Diagnostic #6    '
      udiag(16) = 'user-defined    '
      gdiag(16) = 'SM      ML      '

      cdiag(17) = 'UDIAG7  '
      tdiag(17) = 'User-Defined Upper-Air Diagnostic #7    '
      udiag(17) = 'user-defined    '
      gdiag(17) = 'SM      ML      '

      cdiag(18) = 'UDIAG8  '
      tdiag(18) = 'User-Defined Upper-Air Diagnostic #8    '
      udiag(18) = 'user-defined    '
      gdiag(18) = 'SM      ML      '

      cdiag(19) = 'UDIAG9  '
      tdiag(19) = 'User-Defined Upper-Air Diagnostic #9    '
      udiag(19) = 'user-defined    '
      gdiag(19) = 'SM      ML      '

      cdiag(20) = 'UDIAG10 '
      tdiag(20) = 'User-Defined Upper-Air Diagnostic #10   '
      udiag(20) = 'user-defined    '
      gdiag(20) = 'SM      ML      '

      cdiag(21) = 'SDIAGC  '
      tdiag(21) = 'User-Defined Counted Surface Diagnostic         '
      udiag(21) = 'user-defined    '
      gdiag(21) = 'SM  C   L1      '
      hdiag(21) =  22

      cdiag(22) = 'SDIAGCC '
      tdiag(22) = 'User-Defined Counted Surface Diagnostic Counter'
      udiag(22) = 'count           '
      gdiag(22) = 'SM      L1      '

C-    set the total number of available diagnostics
      ndiagt = 22

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
