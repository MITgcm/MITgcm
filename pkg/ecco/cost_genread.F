C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_genread.F,v 1.7 2015/02/17 14:33:31 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_genread
C     !INTERFACE:
      subroutine cost_genread(
     I                           active_var_file,
     O                           active_var,
     O                           active_tmp,
     I                           iRec,
     I                           nnzbar,
     I                           nrecloc,
     I                           preproc,
     I                           preproc_c,
     I                           preproc_i,
     I                           preproc_r,
     I                           dummy,
     I                           myThid
     &                         )

C     !DESCRIPTION: \bv
C     reads and pre-processes bar file records
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL     active_tmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      INTEGER iRec
      INTEGER myThid
      INTEGER nnzbar, nrecloc
      _RL     dummy
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)
      character*(MAX_LEN_FNAM) preproc_c(NGENPPROC)
      integer preproc_i(NGENPPROC)
      _RL preproc_r(NGENPPROC)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer nyearsINT
      _RL     nyearsRL
      integer iyear, imon
CEOP

      if ( preproc(1) .EQ. ' ') then

         call ecco_readbar( active_var_file, active_var,
     &                      irec, nnzbar, dummy, mythid )

      elseif ( preproc(1) .EQ. 'clim') then

c--   Loop over month
         nyearsINT=int((nrecloc-irec)/preproc_i(1))+1
         nyearsRL=float(nyearsINT)

         call ecco_zero(active_tmp,nnzbar,zeroRL,myThid)

         do iyear=1,nyearsINT
           imon=irec+(iyear-1)*preproc_i(1)
           call ecco_readbar( active_var_file, active_var,
     &                      imon, nnzbar, dummy, mythid )
           call ecco_add(active_var,nnzbar,active_tmp,nnzbar,myThid)
         enddo

         call ecco_div(active_tmp,nnzbar,nyearsRL,myThid)
         call ecco_cp(active_tmp,nnzbar,active_var,nnzbar,myThid)

      endif

#endif /* ALLOW_ECCO */

      RETURN
      END

