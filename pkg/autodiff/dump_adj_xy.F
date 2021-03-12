#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_DIAGNOSTICS
# include "DIAG_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C !ROUTINE: DUMP_ADJ_XY
C !INTERFACE:
      SUBROUTINE DUMP_ADJ_XY(
     I           var2DRS, var2DRL, diagName, dumpName, vType,
     I           doDump, dumpAdRec, myTime, myIter, myThid )

C !DESCRIPTION:
C     Helper subroutine to dump to file and fill corresponding diagnostics
C     for 2-D single variable given the millions of autodiff options

C !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_SIZE.h"
#  include "DIAGNOSTICS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C !INPUT/OUTPUT PARAMETERS:
C   var2DRS ( RS ) :: input 2-D AD-variable field
C   var2DRL ( RL ) :: input 2-D AD-variable field
C   diagName ( C ) :: diagnostics name
C   dumpName ( C ) :: output file prefix
C   vType  ( Int ) :: type of AD-variable (select which ADEXCH to use)
C       vType (1rst digit):
C           = 1,3 : process RS input field ; = 2,4 : process RL input field
C           = 1,2 : without sign. ;          = 3,4 : with sign.
C       vType (2nd digit) = 10 : A-grid location (i.e., grid-cell center)
C                         = 20 : B-grid location (i.e., grid-cell corner)
C   doDump   ( L ) :: do write field to file
C   dumpAdRec (I)  :: record number in file
C   myTime         :: time counter for this thread
C   myIter         :: iteration counter for this thread
C   myThid         :: Thread number for this instance of the routine.
      _RS var2DRS(*)
      _RL var2DRL(*)
      CHARACTER*(8) diagName
      CHARACTER*(*) dumpName
      INTEGER vType
      LOGICAL doDump
      INTEGER dumpAdRec
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C !LOCAL VARIABLES:
C   suff           :: Hold suffix part of a filename
C   var2Dc ( RL )  :: copy of input field
      CHARACTER*(10) suff
      _RL var2Dc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

C-----------------------------------------------------------------------
C--- Output adj variables in diagnostics
C-----------------------------------------------------------------------

C --- 1. Write out dump fields
      IF ( doDump ) THEN

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF

        IF ( dumpAdVarExch.EQ.2 ) THEN

C--       Copy first
          CALL COPY_ADVAR_OUTP( var2DRS, var2DRL,
     &                          var2Dc, 1, vType, myThid )
          IF ( dumpAdByRec ) THEN
            CALL WRITE_REC_XY_RL( dumpName, var2Dc, dumpAdRec,
     &                            myIter, myThid )
          ELSE
            CALL WRITE_FLD_XY_RL( dumpName, suff, var2Dc,
     &                            myIter, myThid )
          ENDIF

        ELSE ! dumpAdVarExch.eq.2

C--       Write directly
          IF ( MOD(vType,2).NE.1 ) THEN
            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XY_RL( dumpName, var2DRL, dumpAdRec,
     &                              myIter, myThid )
            ELSE
              CALL WRITE_FLD_XY_RL( dumpName, suff, var2DRL,
     &                              myIter, myThid )
            ENDIF
          ELSE ! is RL
            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XY_RS( dumpName, var2DRS, dumpAdRec,
     &                              myIter, myThid )
            ELSE
              CALL WRITE_FLD_XY_RS( dumpName, suff, var2DRS,
     &                              myIter, myThid )
            ENDIF
          ENDIF

        ENDIF
      ENDIF

C --- 2. Fill diagnostics
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiag4AdjOutp .AND. diagName.NE.'- None -' ) THEN
        IF ( dumpAdVarExch.EQ.2 ) THEN
          IF ( .NOT.doDump )
     &      CALL COPY_ADVAR_OUTP( var2DRS, var2DRL,
     &                            var2Dc, 1, vType, myThid )
          CALL DIAGNOSTICS_FILL( var2Dc, diagName, 0,1, 0,1,1, myThid )
        ELSE
          IF ( MOD(vType,2).NE.1 ) THEN
            CALL DIAGNOSTICS_FILL( var2DRL, diagName,
     &                             0, 1, 0, 1, 1, myThid )
          ELSE
            CALL DIAGNOSTICS_FILL_RS( var2DRS, diagName,
     &                             0, 1, 0, 1, 1, myThid )
          ENDIF
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
