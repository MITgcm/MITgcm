C $Header: /u/gcmpack/MITgcm/pkg/exch2/exch2_get_scal_bounds.F,v 1.1 2009/05/30 21:22:13 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP 0
C !ROUTINE: EXCH2_GET_SCAL_BOUNDS

C !INTERFACE:
      SUBROUTINE EXCH2_GET_SCAL_BOUNDS(
     I                 fCode, eWdth, updateCorners,
     I                 tgTile, tgNb,
     O                 tIlo, tIhi, tJlo, tJhi,
     O                 tiStride, tjStride,
     I                 myThid )

C !DESCRIPTION:
C     Return the index range & step of the part of the array (overlap-region)
C     which is going to be updated by the exchange with 1 neighbour.
C     Scalar field version (as opposed to 2-components vector field).
C     Note: presently only coded for grid-centered (mass point) scalar.
C           needs to extend it to grid-corner (vorticity point) scalar.

C !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"

C !INPUT/OUTPUT PARAMETERS:
C     fCode         :: field code (position on staggered grid)
C     eWdth         :: width of data region to exchange
C     updateCorners :: flag, do update corner halo region if true
C     tgTile        :: target tile
C     tgNb          :: target Neighbour entry
C     tIlo, tIhi    :: index range in I that will be filled in target array
C     tJlo, tJhi    :: index range in J that will be filled in target array
C     tiStride      :: index step  in I that will be filled in target array
C     tjStride      :: index step  in J that will be filled in target array
C     myThid        :: my Thread Id. number

      CHARACTER*2 fCode
      INTEGER     eWdth
      LOGICAL     updateCorners
      INTEGER     tgTile, tgNb
      INTEGER     tIlo, tIhi, tJlo, tJhi
      INTEGER     tiStride, tjStride
      INTEGER     myThid

C !LOCAL VARIABLES:
CEOP

C---  Initialise index range from Topology values:
      tIlo = exch2_iLo(tgNb,tgTile)
      tIhi = exch2_iHi(tgNb,tgTile)
      tJlo = exch2_jLo(tgNb,tgTile)
      tJhi = exch2_jHi(tgNb,tgTile)

C---  Expand index range according to exchange-Width "eWdth"
      IF ( tIlo .EQ. tIhi .AND. tIlo .EQ. 0 ) THEN
C      Filling a west edge overlap
       tIlo = 1-eWdth
       tiStride = 1
       IF ( tJlo .LE. tJhi ) THEN
        tjStride = 1
       ELSE
        tjStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tJlo = tJlo-tjStride*(eWdth-1)
        tJhi = tJhi+tjStride*(eWdth-1)
       ELSE
        tJlo = tJlo+tjStride
        tJhi = tJhi-tjStride
       ENDIF
      ENDIF
      IF ( tIlo .EQ. tIhi .AND. tIlo .GT. 1 ) THEN
C      Filling an east edge overlap
       tIhi = tIhi+eWdth-1
       tiStride = 1
       IF ( tJlo .LE. tJhi ) THEN
        tjStride = 1
       ELSE
        tjStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tJlo = tJlo-tjStride*(eWdth-1)
        tJhi = tJhi+tjStride*(eWdth-1)
       ELSE
        tJlo = tJlo+tjStride
        tJhi = tJhi-tjStride
       ENDIF
      ENDIF
      IF ( tJlo .EQ. tJhi .AND. tJlo .EQ. 0 ) THEN
C      Filling a south edge overlap
       tJlo = 1-eWdth
       tjStride = 1
       IF ( tIlo .LE. tIhi ) THEN
        tiStride = 1
       ELSE
        tiStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tIlo = tIlo-tiStride*(eWdth-1)
        tIhi = tIhi+tiStride*(eWdth-1)
       ELSE
        tIlo = tIlo+tiStride
        tIhi = tIhi-tiStride
       ENDIF
      ENDIF
      IF ( tJlo .EQ. tJhi .AND. tJlo .GT. 1 ) THEN
C      Filling a north edge overlap
       tJhi = tJhi+eWdth-1
       tjStride = 1
       IF ( tIlo .LE. tIhi ) THEN
        tiStride = 1
       ELSE
        tiStride =-1
       ENDIF
       IF ( updateCorners ) THEN
        tIlo = tIlo-tiStride*(eWdth-1)
        tIhi = tIhi+tiStride*(eWdth-1)
       ELSE
        tIlo = tIlo+tiStride
        tIhi = tIhi-tiStride
       ENDIF
      ENDIF

      RETURN
      END
