C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_DEBUG.h,v 1.1 2006/05/25 17:59:00 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: THSICE_DEBUG.h

C !INTERFACE:
C #include "THSICE_DEBUG.h"
C       LOGICAL dBug
C       dBug(i,j,bi,bj)

C !DESCRIPTION:
C Function used for debugging: define a single grid-point location
C  where values of various variables are printed (to standard-output file)
C
CEOP

#ifdef ALLOW_DBUG_THSICE
      LOGICAL dBug
c     dBug(i,j,bi,bj) = .FALSE.
      dBug(i,j,bi,bj) = dBugFlag .AND.
     &         ( i.EQ.15 .AND. j.EQ.11 .AND. bi.EQ.3 .AND. bj.EQ.1 )
#endif
