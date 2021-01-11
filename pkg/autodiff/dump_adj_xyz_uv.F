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
C     !ROUTINE: DUMP_ADJ_XYZ_UV
C     !INTERFACE:
      SUBROUTINE DUMP_ADJ_XYZ_UV( u3DRS, u3DRL, uDiagName, uDumpName,
     I      v3DRS, v3DRL, vDiagName, vDumpName,
     I      myNr, vType, doDump, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DUMP_ADJ_XYZ_UV                           |
C     *==========================================================*
C     Helper subroutine to dump a single variable given the millions
C     of autodiff options
C     *==========================================================*
C     | SUBROUTINE DUMP_ADJ_XYZ_UV                           |
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
      _RS u3DRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL u3DRL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS v3DRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL v3DRL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(*) uDiagName
      CHARACTER*(*) vDiagName
      CHARACTER*(*) uDumpName
      CHARACTER*(*) vDumpName
      INTEGER myNr
      INTEGER vType
      LOGICAL doDump
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
      _RL uCopy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vCopy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CEOP

C-----------------------------------------------------------------------
C--- Output adj variables in diagnostics
C-----------------------------------------------------------------------

C --- 1. Fill diagnostics
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiag4AdjOutp ) THEN

        IF ( dumpAdVarExch.EQ.2 ) THEN

          CALL COPY_AD_UV_OUTP(u3DRS,v3DRS,u3DRL,v3DRL,
                               uCopy,vCopy,myNr,vType,myThid)
          CALL DIAGNOSTICS_FILL(uCopy,diagName,0,myNr,0,1,1,myThid)
          CALL DIAGNOSTICS_FILL(vCopy,diagName,0,myNr,0,1,1,myThid)
        ELSE
          IF ( MOD(vType,2).NE.1 ) THEN
            CALL DIAGNOSTICS_FILL(u3DRL,diagName,0,myNr,0,1,1,myThid)
            CALL DIAGNOSTICS_FILL(v3DRL,diagName,0,myNr,0,1,1,myThid)
          ELSE
            ! How to handle this case?
            !CALL DIAGNOSTICS_FILL(var3DRS,diagName,0,myNr,0,1,1,myThid)
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
          CALL COPY_AD_UV_OUTP(u3DRS,v3DRS,u3DRL,v3DRL,
                               uCopy,vCopy,myNr,vType,myThid)
          IF ( dumpAdByRec ) THEN
            CALL WRITE_REC_XYZ_RL(dumpName, uCopy, dumpAdRecMn,
     &                            myIter, myThid)
            CALL WRITE_REC_XYZ_RL(dumpName, vCopy, dumpAdRecMn,
     &                            myIter, myThid)

          ELSE
            CALL WRITE_FLD_XYZ_RL(dumpName, suff, uCopy,
                                  myIter, myThid)
            CALL WRITE_FLD_XYZ_RL(dumpName, suff, vCopy,
                                  myIter, myThid)
          ENDIF

        ELSE ! dumpAdVarExch.eq.2

C--       Write directly
          IF ( MOD(vType,2).NE.1 ) THEN

            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XYZ_RL(dumpName, u3DRL, dumpAdRecMn,
     &                              myIter, myThid)
              CALL WRITE_REC_XYZ_RL(dumpName, v3DRL, dumpAdRecMn,
     &                              myIter, myThid)

            ELSE
              CALL WRITE_FLD_XYZ_RL(dumpName, suff,u3DRL,
                                    myIter, myThid)
              CALL WRITE_FLD_XYZ_RL(dumpName, suff,v3DRL,
                                    myIter, myThid)
            ENDIF

          ELSE ! is RL

            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XYZ_RS(dumpName, u3DRS, dumpAdRecMn,
     &                              myIter, myThid)
              CALL WRITE_REC_XYZ_RS(dumpName, v3DRS, dumpAdRecMn,
     &                              myIter, myThid)

            ELSE
              CALL WRITE_FLD_XYZ_RS(dumpName, suff, u3DRS,
                                    myIter, myThid)
              CALL WRITE_FLD_XYZ_RS(dumpName, suff, v3DRS,
                                    myIter, myThid)
            ENDIF
          ENDIF

        ENDIF
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
