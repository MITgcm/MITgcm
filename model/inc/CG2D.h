C $Header: /u/gcmpack/MITgcm/model/inc/CG2D.h,v 1.5 1998/10/28 03:11:35 cnh Exp $
C
C     /==========================================================\
C     | CG2D.h                                                   |
C     | o Two-dimensional conjugate gradient solver header.      |
C     |==========================================================|
C     | The common blocks set up here are used in the elliptic   |
C     | equation inversion. They are also used as the interface  |
C     | to the rest of the model. To set the source term for the |
C     | solver set the appropriate array below. To read the      |
C     | solution read from the appropriate array below.          |
C     \==========================================================/

#include "CG2D_INTERNAL.h"
#include "CG2D_EXTERNAL.h"
