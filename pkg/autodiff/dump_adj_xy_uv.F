#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_DIAGNOSTICS
# include "DIAG_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C !ROUTINE: DUMP_ADJ_XY_UV
C !INTERFACE:
      SUBROUTINE DUMP_ADJ_XY_UV(
     I           u2DRS, u2DRL, uDiagName, uDumpName,
     I           v2DRS, v2DRL, vDiagName, vDumpName,
     I           vType, doDump, dumpAdRec, myTime, myIter, myThid )

C !DESCRIPTION:
C     Helper subroutine to dump to file and fill corresponding diagnostics
C     for 2-D 2-component vector field given the millions of autodiff options

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
C   u2DRS   ( RS ) :: input 2-D, u-component AD-variable field
C   u2DRL   ( RL ) :: input 2-D, u-component AD-variable field
C   uDiagName (C ) :: u-component diagnostics name
C   uDumpName (C ) :: u-component output file prefix
C   v2DRS   ( RS ) :: input 2-D, v-component AD-variable field
C   v2DRL   ( RL ) :: input 2-D, v-component AD-variable field
C   vDiagName (C ) :: v-component diagnostics name
C   vDumpName (C ) :: v-component output file prefix
C   vType  ( Int ) :: type of AD-variable (select which ADEXCH to use)
C       vType (1rst digit):
C           = 1,3 : process RS input field ; = 2,4 : process RL input field
C           = 1,2 : without sign. ;          = 3,4 : with sign.
C       vType (2nd digit) = 10 : A-grid location (i.e., grid-cell center)
C                         = 20 : B-grid location (i.e., grid-cell corner)
C                         = 30 : C-grid location ; = 40 : D-grid location
C   doDump   ( L ) :: do write field to file
C   dumpAdRec (I ) :: record number in file
C   myTime         :: time counter for this thread
C   myIter         :: iteration counter for this thread
C   myThid         :: Thread number for this instance of the routine.
      _RS u2DRS(*)
      _RL u2DRL(*)
      _RS v2DRS(*)
      _RL v2DRL(*)
      CHARACTER*(8) uDiagName
      CHARACTER*(8) vDiagName
      CHARACTER*(*) uDumpName
      CHARACTER*(*) vDumpName
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
C   uCopy  ( RL )  :: copy of u-component input field
C   vCopy  ( RL )  :: copy of v-component input field
      CHARACTER*(10) suff
      _RL uCopy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vCopy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
          CALL COPY_AD_UV_OUTP( u2DRS, v2DRS, u2DRL, v2DRL,
     &                          uCopy, vCopy, 1, vType, myThid )
          IF ( dumpAdByRec ) THEN
            CALL WRITE_REC_XY_RL( uDumpName, uCopy, dumpAdRec,
     &                            myIter, myThid )
            CALL WRITE_REC_XY_RL( vDumpName, vCopy, dumpAdRec,
     &                            myIter, myThid )
          ELSE
            CALL WRITE_FLD_XY_RL( uDumpName, suff, uCopy,
     &                            myIter, myThid )
            CALL WRITE_FLD_XY_RL( vDumpName, suff, vCopy,
     &                            myIter, myThid )
          ENDIF

        ELSE ! dumpAdVarExch.eq.2

C--       Write directly
          IF ( MOD(vType,2).NE.1 ) THEN
            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XY_RL( uDumpName, u2DRL, dumpAdRec,
     &                              myIter, myThid )
              CALL WRITE_REC_XY_RL( vDumpName, v2DRL, dumpAdRec,
     &                              myIter, myThid )
            ELSE
              CALL WRITE_FLD_XY_RL( uDumpName, suff, u2DRL,
     &                              myIter, myThid )
              CALL WRITE_FLD_XY_RL( vDumpName, suff, v2DRL,
     &                              myIter, myThid )
            ENDIF
          ELSE ! is RL
            IF ( dumpAdByRec ) THEN
              CALL WRITE_REC_XY_RS( uDumpName, u2DRS, dumpAdRec,
     &                              myIter, myThid )
              CALL WRITE_REC_XY_RS( vDumpName, v2DRS, dumpAdRec,
     &                              myIter, myThid )
            ELSE
              CALL WRITE_FLD_XY_RS( uDumpName, suff, u2DRS,
     &                              myIter, myThid )
              CALL WRITE_FLD_XY_RS( vDumpName, suff, v2DRS,
     &                              myIter, myThid )
            ENDIF
          ENDIF

        ENDIF
      ENDIF

C --- 2. Fill diagnostics
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiag4AdjOutp .AND. uDiagName.NE.'- None -'
     &                     .AND. vDiagName.NE.'- None -' ) THEN
        IF ( dumpAdVarExch.EQ.2 ) THEN
          IF ( .NOT.doDump )
     &      CALL COPY_AD_UV_OUTP( u2DRS, v2DRS, u2DRL, v2DRL,
     &                            uCopy, vCopy, 1, vType, myThid )
          CALL DIAGNOSTICS_FILL( uCopy, uDiagName, 0,1, 0,1,1, myThid )
          CALL DIAGNOSTICS_FILL( vCopy, vDiagName, 0,1, 0,1,1, myThid )
        ELSE
          IF ( MOD(vType,2).NE.1 ) THEN
            CALL DIAGNOSTICS_FILL( u2DRL, uDiagName,
     &                             0, 1, 0, 1, 1, myThid )
            CALL DIAGNOSTICS_FILL( v2DRL, vDiagName,
     &                             0, 1, 0, 1, 1, myThid )
          ELSE
            CALL DIAGNOSTICS_FILL_RS( u2DRS, uDiagName,
     &                             0, 1, 0, 1, 1, myThid )
            CALL DIAGNOSTICS_FILL_RS( v2DRS, vDiagName,
     &                             0, 1, 0, 1, 1, myThid )
          ENDIF
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
