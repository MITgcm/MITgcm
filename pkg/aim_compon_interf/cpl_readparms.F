C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_readparms.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_READPARMS
C     !INTERFACE:
      SUBROUTINE CPL_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CPL_READPARMS
C     | o Read Coupling parameters that control import/export
C     |   from/to the coupler layer
C     *==========================================================*
C     |   this version is specific to 1 component (atmos)
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE

C     == Global variables ===

#include "SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==       
C     myThid -  Number of this instance
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE

C Functions
      INTEGER ILNBLNK

C     == Local Variables == 
C     msgBuf     :: Informational/error meesage buffer
C     iUnit      :: Work variable for IO unit number
C     k          :: loop counter
C     iL         :: Work variable for length of file-name
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit, k, iL
      _RL  cpl_atmSendFrq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Coupling parameters: 
C     useImportSST   :: True => use the Imported SST from couler
C     cpl_atmSendFrq :: Frequency^-1 for sending data to coupler (s) 
      NAMELIST /CPL_ATM_PARAM/
     &    useImportSST,
     &    cpl_atmSendFrq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Set default value:
      useImportSST = .TRUE. 
      cpl_atmSendFrq  = deltaTClock
      
      _BEGIN_MASTER(myThid)
      
      WRITE(msgBuf,'(A)') ' CPL_READPARMS: opening data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE( 'data.cpl', 'CPL_READPARMS',
     O                          iUnit, myThid )

C--   Read parameters from open data file:

C-    Parameters for coupling interface:
      READ(UNIT=iUnit,NML=CPL_ATM_PARAM)

      WRITE(msgBuf,'(A)') 
     &   ' CPL_READPARMS: finished reading data.cpl'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
 
C--   Close the open data file
      CLOSE(iUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check parameters and model configuration

C-    derive other parameters:
      cplSendFrq_iter = NINT( cpl_atmSendFrq / deltaTClock )
      IF ( cplSendFrq_iter .LT. 1) cplSendFrq_iter = 1

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Print out parameter values :

      iUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// Coupling package parameters :'
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE(msgBuf,iUnit,SQUEEZE_RIGHT,1)

C- namelist CPL_ATM_PARAM:
       CALL WRITE_0D_L( useImportSST , INDEX_NONE,
     &                 'useImportSST =',
     &   ' /* use Imported SST from Coupler on/off flag */')       
       CALL WRITE_0D_R8( cpl_atmSendFrq, INDEX_NONE, 'cpl_atmSendFrq =',
     &   ' /* Frequency^o-1 for sending data to Coupler (s) */')
C     cpl_atmSendFrq  :: Frequency^-1 for sending data to coupler (s) 
       CALL WRITE_0D_I( cplSendFrq_iter, INDEX_NONE,'cplSendFrq_iter =',
     &'   /* send data to coupler every "cplSendFrq" iter */')


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)
 
C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
