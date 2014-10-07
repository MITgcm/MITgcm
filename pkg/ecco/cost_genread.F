C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_genread.F,v 1.5 2014/10/07 14:45:53 gforget Exp $
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
     I                           nnzbar,
     I                           preproc,
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

C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      INTEGER iRec
      INTEGER myThid
      INTEGER nnzbar
      _RL     dummy
      character*(MAX_LEN_FNAM) preproc(NGENPPROC)

#ifdef ALLOW_ECCO

c     == local variables ==

      integer nyearsINT
      _RL     nyearsRL
      _RL     fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL     fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      integer iyear, imon
CEOP

      if ( preproc(1) .EQ. ' ') then

         call ecco_readbar( active_var_file, active_var,
     &                      irec, nnzbar, dummy, mythid )

      elseif ( preproc(1) .EQ. 'climmon') then

c--   Loop over month
         nyearsINT=int((nmonsrec-irec)/12)+1
         nyearsRL=float(nyearsINT)

         call ecco_zero(fld1,nnzbar,zeroRL,myThid)

         do iyear=1,nyears
           imon=irec+(iyear-1)*12
           call ecco_readbar( active_var_file, fld2,
     &                      imon, nnzbar, dummy, mythid )
           call ecco_add(fld2,nnzbar,fld1,nnzbar,myThid)
         enddo

         call ecco_div(fld1,nnzbar,nyearsRL,myThid)
         call ecco_cp(fld1,nr,active_var,nnzbar,myThid)

      endif

#endif /* ALLOW_ECCO */

      RETURN
      END

