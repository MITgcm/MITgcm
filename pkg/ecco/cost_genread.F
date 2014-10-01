C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_genread.F,v 1.1 2014/10/01 12:54:03 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_genread
C     !INTERFACE:
      subroutine cost_genread(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           nnzbar,
     I                           preproc,
     I                           myThid,
     I                           dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE cost_genread
C     ==================================================================
C     reads and pre-processes bar file records
C     ==================================================================
C     SUBROUTINE cost_genread
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER nnzbar
      _RL     dummy
      character*(8) preproc

C     !LOCAL VARIABLES:
CEOP

cgf here the read sequence will depend on preproc (e.g. as GENERIC_BAR_MONTH)

        if ( nnzbar .EQ. 1 ) then
           call active_read_xy( active_var_file, active_var,
     &                      irec, doglobalread,
     &                      ladinit, eccoiter, mythid,
     &                      dummy )
        else
           call active_read_xyz( active_var_file, active_var,
     &                       irec, doglobalread,
     &                       ladinit, eccoiter, mythid,
     &                       dummy )
        endif

      RETURN
      END

