C $Header: /u/gcmpack/MITgcm/eesupp/src/nml_set_terminator.F,v 1.4 2010/12/26 02:59:37 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: NML_SET_TERMINATOR

C     !INTERFACE:
      SUBROUTINE NML_SET_TERMINATOR(
     U                               record )

C     !DESCRIPTION:
C     *=================================================================*
C     | SUBROUTINE NML\_SET\_TERMINATOR
C     | o Set namelist terminator character
C     *=================================================================*
C     | Change trailing \& to trailing / when needed
C     *=================================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "EEPARAMS.h"

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      CHARACTER*(MAX_LEN_PREC) record

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER iL
      CHARACTER*(2) nmlEnd
#ifdef NML_TERMINATOR
      PARAMETER( nmlEnd = ' /' )
#else
      PARAMETER( nmlEnd = ' &' )
#endif
CEOP

      iL = MAX(ILNBLNK(record),1)
      IF ( iL .EQ. 2 ) THEN
       IF ( record(1:2) .EQ. ' &' ) THEN
        record(1:2) = nmlEnd
       ENDIF
      ENDIF

      RETURN
      END
