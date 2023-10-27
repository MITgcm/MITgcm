CBOP
C     !ROUTINE: OPTIMCYCLE.h
C     !INTERFACE:
C     #include "OPTIMCYCLE.h"

C     !DESCRIPTION:
C     *================================================================*
C     | OPTIMCYCLE.h
C     | o Header file defining variable optimcycle and common block
C     | o optimcycle is the only parameter of the offline optimization
C     |   toolbox "optim" (or "https://github.com/mjlosch/optim_m1qn3")
C     |   that is used in the MITgcm
C     | o formerly part of optim.h
C     *================================================================*
CEOP

C     optimcycle :: cycle number of the off-line optimization.
      COMMON /optiparm_i/ optimcycle
      INTEGER optimcycle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
