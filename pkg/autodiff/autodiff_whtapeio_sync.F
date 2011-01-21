C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_whtapeio_sync.F,v 1.1 2011/01/21 01:19:03 gforget Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine autodiff_whtapeio_sync( myLev, myStep, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE autodiff_whtapeio_sync                             |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ctrl.h"
#include "AUTODIFF.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myLev
      integer myStep

#ifdef ALLOW_AUTODIFF_WHTAPEIO

      character*(MAX_LEN_FNAM) fName
      integer filePrec, IL, length_of_rec
      LOGICAL iAmDoingIO

C     sNxWh :: x tile size with halo included
C     sNyWh :: y tile size with halo included
C     pocNyWh :: processor sum of sNyWh
C     gloNyWh :: global sum of sNyWh
      INTEGER sNxWh
      INTEGER sNyWh
      INTEGER procNyWh
      INTEGER gloNyWh
      PARAMETER ( sNxWh = sNx+2*Olx )
      PARAMETER ( sNyWh = sNy+2*Oly )
      PARAMETER ( procNyWh = sNyWh*nSy*nSx )
      PARAMETER ( gloNyWh = procNyWh*nPy*nPx )

c     == functions ==
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

      IF ( tapeConcatIO ) THEN

      IF ( doSinglePrecTapelev ) THEN
       filePrec = 32
      ELSE
       filePrec = 64
      ENDIF      

C Only do I/O if I am the master thread (and mpi process 0 IF tapeSingleCpuIO):
      iAmDoingIO = .FALSE.
      IF ( .NOT.tapeSingleCpuIO .OR. myProcId.EQ.0 ) THEN
        _BEGIN_MASTER( myThid )
        iAmDoingIO = .TRUE.
        _END_MASTER( myThid )
      ENDIF      
      
      IF ( iAmDoingIO ) THEN
        IF ( .NOT.tapeSingleCpuIO ) THEN
          WRITE(fName,'(A,I1.1,A,I3.3,A)') 
     &         'tapes',myLev,'.',myProcId,'.data'
          length_of_rec = MDS_RECLEN( filePrec,sNxWh*procNyWh,myThid )
        ELSE
          WRITE(fName,'(A,I1.1,A)') 'tapes',myLev,'.data'
          length_of_rec = MDS_RECLEN( filePrec,sNxWh*gloNyWh,myThid)
        ENDIF
      ENDIF

      tapeFileUnit=tapeFileUnitS(myLev)
      tapeFileCounter=0

      IF ( iAmDoingIO.AND.(myStep.EQ.0).AND.
     &    (myLev.GT.0).AND.(tapeFileUnit.EQ.0) ) THEN
        CALL MDSFINDUNIT( tapeFileUnit, myThid )
        print*,'cgf opening :',tapeFileUnit,fName
        OPEN( tapeFileUnit, file=fName, status='unknown',
     &         access='direct', recl=length_of_rec )
        tapeFileUnitS(myLev)=tapeFileUnit
      ENDIF

      ENDIF !IF ( tapeConcatIO ) THEN

#endif /* ALLOW_AUTODIFF_WHTAPEIO */

      end
