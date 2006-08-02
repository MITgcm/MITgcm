C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_atmos.F,v 1.4 2006/08/02 22:51:24 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CFC_ATMOS( myThid )
C     *==========================================================*
C     | SUBROUTINE CFC_ATMOS
C     | o read in timeseries of atmoshperic CFC
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CFC.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of CFC_ATMOS
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_CFC

C     == Local variables ==
      integer  nTimePnts, iUnit, it
      integer i,j,bi,bj
      _RL year(100)
C     msgBuf     :: message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C jmc: a better place to initialise those arrays would be in GCHEM_INIT_VARI
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           AtmosCFC11(i,j,bi,bj)=0. _d 0
           AtmosCFC12(i,j,bi,bj)=0. _d 0
         ENDDO
         ENDDO
        ENDDO
        ENDDO

      _BEGIN_MASTER( mythid )

      WRITE(msgBuf,'(A)')
     &       'S/R CFC_ATMOS: reading CFC atmospheric data'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)

C read in CFC atmospheric timeseries data
      cfc_yearbeg = 31
      cfc_yearend = 98 + 1
      nTimePnts=cfc_yearend-cfc_yearbeg
C assign a free unit number as the I/O channel for this subroutine
      CALL MDSFINDUNIT( iUnit, mythid )
      OPEN(iUnit,FILE='cfc1112.atm',STATUS='old')
C skip 6 descriptor lines
      DO i =1,6
          READ(iUnit,*)
      ENDDO
C Read in CFC11 and CFC12, N and S Hemisphere time histories
      DO it = 1,nTimePnts
         READ(iUnit,*) year(it),ACFC11(it,1),ACFC12(it,1),
     &        ACFC11(it,2),ACFC12(it,2)
         WRITE(msgBuf,'(A,4F8.2)')
     &        'year,acfc11_n,acfc12_n,acfc11_s,acfc12_s =',
     &        ACFC11(it,1),ACFC12(it,1),
     &        ACFC11(it,2),ACFC12(it,2)
         CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)

      ENDDO
      CLOSE(iUnit)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_CFC */

      RETURN
      END
