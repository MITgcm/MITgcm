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
     I                           nz,
     I                           nnzbar,
     I                           nrecloc,
     I                           preproc,
     I                           preproc_c,
     I                           preproc_i,
     I                           preproc_r,
     I                           dummy,
     I                           myThid )

C     !DESCRIPTION: \bv
C     reads and pre-processes bar file records
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nz,nSx,nSy)
      _RL     active_tmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nz,nSx,nSy)
      INTEGER iRec
      INTEGER nz, nnzbar, nrecloc
      CHARACTER*(MAX_LEN_FNAM) preproc(NGENPPROC)
      CHARACTER*(MAX_LEN_FNAM) preproc_c(NGENPPROC)
      INTEGER preproc_i(NGENPPROC)
      _RL     preproc_r(NGENPPROC)
      _RL     dummy
      INTEGER myThid

#ifdef ALLOW_ECCO
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER nyearsINT
      _RL     recip_nyearsRL
      INTEGER iyear, imon
      INTEGER i,j,k,bi,bj
CEOP

      IF ( preproc(1) .EQ. 'clim') THEN

c--   Loop over month
       nyearsINT=int((nrecloc-irec)/preproc_i(1))+1
       recip_nyearsRL = nyearsINT
       recip_nyearsRL = 1. _d 0/recip_nyearsRL

       CALL ECCO_ZERO( active_var, nz, zeroRL, myThid )
       CALL ECCO_ZERO( active_tmp, nz, zeroRL, myThid )
       DO iyear=1,nyearsINT
        imon=irec+(iyear-1)*preproc_i(1)
        CALL ECCO_READBAR( active_var_file, active_var,
     &       imon, nz, nnzbar, dummy, myThid )
        DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k = 1,nnzbar
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             active_tmp(i,j,k,bi,bj) = active_tmp(i,j,k,bi,bj)
     &            + active_var(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO k = 1,nnzbar
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
            active_var(i,j,k,bi,bj) = active_tmp(i,j,k,bi,bj)
     &           * recip_nyearsRL
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       CALL ECCO_READBAR( active_var_file, active_var,
     &      irec, nz, nnzbar, dummy, myThid )
      ENDIF

#endif /* ALLOW_ECCO */

      RETURN
      END
