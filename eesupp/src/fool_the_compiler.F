C $Header: /u/gcmpack/MITgcm/eesupp/src/fool_the_compiler.F,v 1.10 2014/07/15 22:00:28 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--  File fool_the_compiler.F: Routines to prevent compiler to skip pieces of code
C--   Contents
C--   o FOOL_THE_COMPILER
C--   o FOOL_THE_COMPILER_R8
C--   o FOOL_THE_COMPILER_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP

C      !ROUTINE: FOOL_THE_COMPILER

C      !INTERFACE:
       SUBROUTINE FOOL_THE_COMPILER( foolishArg )
C      !DESCRIPTION:
C      *=================================================================*
C      | SUBROUTINE FOOL\_THE\_COMPILER
C      | o Dummy routine used to supress compiler behavior.
C      *=================================================================*
C      | Routine is used to stop compiler optimizations in which global
C      | data is held in a register while it is updated by a separate
C      | thread. Calling this routine introduces the possibility of side
C      | effects on global data ensuring that memory locs. are reread.
C      *=================================================================*
       IMPLICIT NONE
CEOP
       INTEGER foolishArg
       RETURN
       END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP

C      !ROUTINE: FOOL_THE_COMPILER_R8

C      !INTERFACE:
       SUBROUTINE FOOL_THE_COMPILER_R8( foolishArg )
C      !DESCRIPTION:
C      *=================================================================*
C      | SUBROUTINE FOOL\_THE\_COMPILER\_R8
C      | o Dummy routine used to supress compiler behavior.
C      *=================================================================*
C      | Routine is used to stop compiler optimizations in which global
C      | data is held in a register while it is updated by a separate
C      | thread. Calling this routine introduces the possibility of side
C      | effects on global data ensuring that memory locs. are reread.
C      *=================================================================*
       IMPLICIT NONE
CEOP
       Real*8 foolishArg
       RETURN
       END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP

C      !ROUTINE: FOOL_THE_COMPILER_RL

C      !INTERFACE:
       SUBROUTINE FOOL_THE_COMPILER_RL( foolishArg )
C      !DESCRIPTION:
C      *=================================================================*
C      | SUBROUTINE FOOL\_THE\_COMPILER\_RL
C      | o Dummy routine used to supress compiler behavior.
C      *=================================================================*
C      | Routine is used to stop compiler optimizations in which global
C      | data is held in a register while it is updated by a separate
C      | thread. Calling this routine introduces the possibility of side
C      | effects on global data ensuring that memory locs. are reread.
C      *=================================================================*
       IMPLICIT NONE
CEOP
       _RL    foolishArg
       RETURN
       END
