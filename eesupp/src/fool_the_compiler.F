C $Header: /u/gcmpack/MITgcm/eesupp/src/fool_the_compiler.F,v 1.6 2001/09/21 03:54:35 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP

C      !ROUTINE: FOOL_THE_COMPILER

C      !INTERFACE:
       SUBROUTINE FOOL_THE_COMPILER
       IMPLICIT NONE
C      !DESCRIPTION:
C      *=================================================================*
C      | SUBROUTINE FOOL_THE_COMPILER                                    |
C      | o Dummy routine used to supress compiler behavior.              |
C      *=================================================================*
C      | Routine is used to stop compiler optimizations in which global  |
C      | data is held in a register while it is updated by a separate    |
C      | thread. Calling this routine introduces the possibility of side |
C      | effects on global data ensuring that memory locs. are reread.   |
C      *=================================================================*
CEOP
       RETURN
       END
