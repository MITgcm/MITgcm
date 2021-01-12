#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_DIAGNOSTICS
# include "DIAG_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: DUMP_ADJ_XYZ
C     !INTERFACE:
      SUBROUTINE DUMP_ADJ_XYZ( var3DRS, var3DRL, diagName, dumpName,
     I      vType, doDump, dumpAdRec, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DUMP_ADJ_XYZ                                  |
C     *==========================================================*
C     Helper subroutine to dump a single variable given the
C     millions of autodiff options
C     *==========================================================*
C     | SUBROUTINE DUMP_ADJ_XYZ                                  |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#ifdef ALLOW_OPENAD
      use OAD_active
      use OAD_rev
      use OAD_tape
      use OAD_cp
#endif
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF.h"
# ifndef ALLOW_OPENAD
#  include "adcommon.h"
# endif /* ALLOW_OPENAD */
# ifdef ALLOW_MNC
#  include "MNC_PARAMS.h"
# endif
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_SIZE.h"
#  include "DIAGNOSTICS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: Thread number for this instance of the routine.
      _RS var3DRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL var3DRL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(*) diagName
      CHARACTER*(*) dumpName
      INTEGER vType
      LOGICAL doDump
      INTEGER dumpAdRec
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff    :: Hold suffix part of a filename
C     msgBuf  :: Error message buffer
      CHARACTER*(10) suff
      _RL var3Dc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CEOP

C-----------------------------------------------------------------------
C--- Output adj variables in diagnostics
C-----------------------------------------------------------------------

C --- 1. Fill diagnostics
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiag4AdjOutp ) THEN

        IF ( dumpAdVarExch.EQ.2 ) THEN

          CALL COPY_ADVAR_OUTP(var3DRS,var3DRL,var3Dc,Nr,vType,myThid)
          CALL DIAGNOSTICS_FILL(var3Dc,diagName,0,Nr,0,1,1,myThid)
        ELSE
          IF ( MOD(vType,2).NE.1 ) THEN
            CALL DIAGNOSTICS_FILL(var3DRL,diagName,0,Nr,0,1,1,myThid)
          ELSE
            CALL DIAGNOSTICS_FILL_RS(var3DRS,diagName,0,Nr,0,1,1,myThid)
          ENDIF
        ENDIF
      ENDIF
#endif

C --- 2. Write out dump fields
      IF ( doDump ) THEN

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF

        IF ( dumpAdVarExch.EQ.2 ) THEN

C--       Copy first
          CALL COPY_ADVAR_OUTP(var3DRS,var3DRL,var3Dc,Nr,vType,myThid)
          IF ( dumpAdByRec ) THEN
            CALL WRITE_REC_XYZ_RL(dumpName, var3Dc, dumpAdRec,
     &                            myIter, myThid)

          ELSE
            CALL WRITE_FLD_XYZ_RL(dumpName, suff,var3Dc,
     &                            myIter, myThid)
          ENDIF

        ELSE ! dumpAdVarExch.eq.2

C--       Write directly
          IF ( MOD(vType,2).NE.1 ) THEN

            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XYZ_RL(dumpName, var3DRL, dumpAdRec,
     &                              myIter, myThid)

            ELSE
              CALL WRITE_FLD_XYZ_RL(dumpName, suff,var3DRL,
     &                              myIter, myThid)
            ENDIF

          ELSE ! is RL

            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XYZ_RS(dumpName, var3DRS, dumpAdRec,
     &                              myIter, myThid)

            ELSE
              CALL WRITE_FLD_XYZ_RS(dumpName, suff,var3DRS,
     &                              myIter, myThid)
            ENDIF
          ENDIF

        ENDIF
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
