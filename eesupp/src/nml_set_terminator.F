C $Header: /u/gcmpack/MITgcm/eesupp/src/nml_set_terminator.F,v 1.3 2004/03/27 03:51:51 edhill Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

#define FTN_NML_F90
#ifndef NML_TERMINATOR
#define NML_TERMINATOR  ' &'
#else
#define NML_TERMINATOR  ' /'
#endif

CBOP

C     !ROUTINE: NML_SET_TERMINATOR

C     !INTERFACE:
      SUBROUTINE NML_SET_TERMINATOR(
     I                       record
     &                    )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *=================================================================*
C     | SUBROUTINE NML\_SET\_TERMINATOR
C     | o Set namelist terminator character
C     *=================================================================*
C     | Change trailing \& to trailing / when needed
C     *=================================================================*

C     !USES:
C     == Global variables ==
#include "EEPARAMS.h"
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      CHARACTER*(MAX_LEN_PREC) record

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER il
CEOP

      IL = MAX(ILNBLNK(RECORD),1)
      IF ( IL .EQ. 2 ) THEN
       IF ( RECORD(1:2) .EQ. ' &' ) THEN
        RECORD(1:2) = NML_TERMINATOR
       ENDIF
      ENDIF
C
      RETURN
      END

