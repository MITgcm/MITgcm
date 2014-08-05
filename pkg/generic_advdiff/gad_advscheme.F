C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_advscheme.F,v 1.1 2014/08/05 01:50:35 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

C--   File gad_advscheme.F: Keeping track of Advection schemes requirement
C--    Contents:
C--    o GAD_ADVSCHEME_INIT
C--    o GAD_ADVSCHEME_SET
C--    o GAD_ADVSCHEME_GET (Function)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GAD_ADVSCHEME_INIT

C     !INTERFACE:
      SUBROUTINE GAD_ADVSCHEME_INIT( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R GAD\_ADVSCHEME\_INIT
C     | o Initialise shared local variables
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C--   Global variables
#include "SIZE.h"
#include "GAD.h"

C--   Local variables shared by S/R within this file (gad_advscheme.F)
C     GAD_advScheme_OlMin :: overlap minimum size for this advection scheme
      INTEGER GAD_advScheme_OlMin(GAD_Scheme_MaxNum)
      COMMON /GAD_ADVSCHEME_LOCAL/ GAD_advScheme_OlMin

C     !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER n
CEOP

      _BEGIN_MASTER(myThid)
       DO n=1,GAD_Scheme_MaxNum
         GAD_advScheme_OlMin(n) = -1
       ENDDO
      _END_MASTER(myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GAD_ADVSCHEME_SET

C     !INTERFACE:
      SUBROUTINE GAD_ADVSCHEME_SET(
     I                         advScheme, OlMinSize,
     U                         errCode,
     I                         myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R GAD\_ADVSCHEME\_SET
C     | o Store minimum length of OverLap (related to stencil)
C     |   that this advection needs.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C--   Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GAD.h"

C--   Local variables shared by S/R within this file (gad_advscheme.F)
C     GAD_advScheme_OlMin :: overlap minimum size for this advection scheme
      INTEGER GAD_advScheme_OlMin(GAD_Scheme_MaxNum)
      COMMON /GAD_ADVSCHEME_LOCAL/ GAD_advScheme_OlMin

C     !INPUT/OUTPUT PARAMETERS:
C     advScheme :: advection scheme number
C     OlMinSize :: overlap minimum size for this advection scheme
C     errCode   :: tracks occurence of errors ( > 0)
C     myThid    :: my Thread Id number
      INTEGER advScheme
      INTEGER OlMinSize
      INTEGER errCode
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf  :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( advScheme.GE.1 .AND. advScheme.LE.GAD_Scheme_MaxNum ) THEN
       _BEGIN_MASTER(myThid)
        GAD_advScheme_OlMin(advScheme) = OlMinSize
       _END_MASTER(myThid)
      ELSE
        WRITE(msgBuf,'(2A,I6,A)') 'GAD_ADVSCHEME_SET: ',
     &    'Advection-Scheme Number:', advScheme, ' not valid'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I4,A)') 'GAD_ADVSCHEME_SET: ',
     &    'should be > 0 and < GAD_Scheme_MaxNum =', GAD_Scheme_MaxNum
        CALL PRINT_ERROR( msgBuf, myThid )
        errCode = MAX( errCode, 1 )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GAD_ADVSCHEME_GET

C     !INTERFACE:
      INTEGER FUNCTION GAD_ADVSCHEME_GET( advScheme )

C     !DESCRIPTION:
C     *==========================================================*
C     | INTEGER FUNCTION GAD\_ADVSCHEME\_GET
C     | o Return minimum length of OverLap (related to stencil)
C     |   that this advection needs.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C--   Global variables
#include "SIZE.h"
#include "GAD.h"

C--   Local variables shared by S/R within this file (gad_advscheme.F)
C     GAD_advScheme_OlMin :: overlap minimum size for this advection scheme
      INTEGER GAD_advScheme_OlMin(GAD_Scheme_MaxNum)
      COMMON /GAD_ADVSCHEME_LOCAL/ GAD_advScheme_OlMin

C     !INPUT PARAMETERS:
C     advScheme  :: advection scheme number
      INTEGER advScheme

C     !LOCAL VARIABLES:
CEOP

      IF ( advScheme.GE.1 .AND. advScheme.LE.GAD_Scheme_MaxNum ) THEN
        GAD_ADVSCHEME_GET = GAD_advScheme_OlMin(advScheme)
      ELSE
        GAD_ADVSCHEME_GET = -2
      ENDIF

      RETURN
      END
