C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_ascii_out.F,v 1.6 2014/07/14 23:26:03 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGSTATS_ASCII_OUT

C     !INTERFACE:
      SUBROUTINE DIAGSTATS_ASCII_OUT(
     I                    statGlob, nLev, ndId,
     I                    mId, listId, myIter, myThid )

C     !DESCRIPTION:
C     Write Global statistic to ASCII file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     statGlob ..... AVERAGED DIAGNOSTIC QUANTITY
C     nLev   ....  2nd Dimension (max Nb of levels) of statGlob array
C     ndId     ... diagnostic Id number (in diagnostics long list)
C     mId    ..... field rank in list "listId"
C     listId ..... current output Stream list
C     myIter ..... current Iteration Number
C     myThid ..... my thread Id number
      INTEGER nLev
      _RL statGlob(0:nStats,0:nLev,0:nRegions)
      INTEGER ndId, mId, listId
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER im, ix, iv
      PARAMETER ( iv = nStats - 2 , im = nStats - 1 , ix = nStats )
      INTEGER i, j, k, klev, nUnit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER( myThid)

#ifdef ALLOW_USE_MPI
      IF ( diagSt_Ascii .AND. mpiMyId.EQ.0 ) THEN
#else
      IF ( diagSt_Ascii ) THEN
#endif

       nUnit = diagSt_ioUnit(listId)
       klev = kdiag(ndId)
C-     single level field: Vertical Integral (k=0) & 1rst level are identical
C      => write only 1 of the 2:
       IF ( klev.EQ.1 ) kLev = 0
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
       DO j=0,nRegions
        IF ( diagSt_region(j,listId).GE.1 ) THEN
         WRITE(nUnit,'(3A,I10,2(A,I4))') ' field : ', cdiag(ndId),
     &    ' ; Iter =',myIter,' ; region #',j, ' ; nb.Lev =',kdiag(ndId)
c        WRITE(nUnit,'(5A)')       ' k |',
c    &             ' --   Average  --  |', ' --   Std.Dev   -- |',
c    &             ' --      min   --  |', ' --      max    -- |'
c        WRITE(nUnit,'(6A)')     ' k |',
c    &             ' --  Average  -- |', ' --  Std.Dev  -- |',
c    &             ' --    min    -- |', ' --    max    -- |',' --  Vol'
         WRITE(nUnit,'(6A)') ' k |',
     &     ' --   Average    -- |', ' --   Std.Dev    -- |',
     &     ' --     min      -- |', ' --     max      -- |',' --    Vol'
         DO k=0,klev
C     full precision, do not write the volume:
c         WRITE(nUnit,'(I3,1P4E20.12)') k,(statGlob(i,k,j),i=1,nStats)
C     reduced precision + write the volume (usefull for testing):
c         WRITE(nUnit,'(I3,1P5E18.10)')
c    &       k, (statGlob(i,k,j),i=1,nStats), statGlob(0,k,j)
C     full precision + write the volume:
          WRITE(nUnit,'(I3,1P5E21.13)')
     &       k, (statGlob(i,k,j),i=1,nStats), statGlob(0,k,j)
         ENDDO
        ENDIF
       ENDDO
       WRITE(nUnit,'(A)') ' '
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
