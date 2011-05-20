C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_ini_io.F,v 1.1 2011/05/20 22:23:53 gforget Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "PROFILES_OPTIONS.h"

CBOP
C     !ROUTINE: PROFILES_INI_IO
C     !INTERFACE:
      SUBROUTINE PROFILES_INI_IO( myThid )

C     !DESCRIPTION: \bv
C     autodiff_ini_model_io() is where run-time/experiment specific data are
C     passed to any I/O packages ready that will be used for I/O of model state
C     variables.
C     This is specifically for setting up once only information such as
C     shape/size of variables, units, etc... and is only for state variables.
C     \ev

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "profiles.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER iL, pIL
CEOP

      IF ( profilesDir .NE. ' ' ) THEN
       iL = ILNBLNK( profilesDir )
C      append "/", if necessay
       IF ( iL.LT.MAX_LEN_FNAM .AND. profilesDir(iL:iL).NE.'/' ) THEN
        namBuf(1:iL) = profilesDir(1:iL)
        WRITE(profilesDir(1:iL+1),'(2A)') namBuf(1:iL),'/'
       ENDIF
C      create directory
       iL = ILNBLNK( profilesDir ) -1
       WRITE(namBuf,'(2A)') ' mkdir -p ', profilesDir(1:iL)
       pIL = 1 + ILNBLNK( namBuf )
       WRITE(standardMessageUnit,'(3A)')
     & '==> SYSTEM CALL (from INI_MODEL_IO): >',namBuf(1:pIL),'<'
       CALL SYSTEM( namBuf(1:pIL) )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
