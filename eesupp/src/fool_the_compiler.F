C $Header: /u/gcmpack/MITgcm/eesupp/src/fool_the_compiler.F,v 1.8 2004/03/27 03:51:51 edhill Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP

C      !ROUTINE: FOOL_THE_COMPILER

C      !INTERFACE:
       SUBROUTINE FOOL_THE_COMPILER( foolishArg )
       IMPLICIT NONE
C      !DESCRIPTION:
C      *=================================================================*
C      | SUBROUTINE FOOL\_THE\_COMPILER                                  |
C      | o Dummy routine used to supress compiler behavior.              |
C      *=================================================================*
C      | Routine is used to stop compiler optimizations in which global  |
C      | data is held in a register while it is updated by a separate    |
C      | thread. Calling this routine introduces the possibility of side |
C      | effects on global data ensuring that memory locs. are reread.   |
C      *=================================================================*
CEOP
       INTEGER foolishArg
       RETURN
       END
