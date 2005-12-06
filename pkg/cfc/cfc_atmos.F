
#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CFC_ATMOS( myThid )
C     /==========================================================\
C     | SUBROUTINE CFC_ATMOS                                    |
C     | o read in timeseries of atmoshperic CFC 
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "CFC.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_TR1
      INTEGER myThid
      integer i,j,bi,bj
CEndOfInterface

C     == Local variables ==
      integer  nTimePnts, Iunit, it
      _RL year(100)
C     msgBuf     :: message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_CFC

        _BEGIN_MASTER( mythid )
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           AtmosCFC11(i,j,bi,bj)=0.d0
           AtmosCFC12(i,j,bi,bj)=0.d0
         ENDDO
         ENDDO
        ENDDO
        ENDDO
        WRITE(msgBuf,'(A)') 
     &       'S/R CFC_ATMOS: reading CFC atmospheric data'
        CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , 1)
c
C read in CFC atmospheric timeseries data
      cfc_yearbeg = 31
      cfc_yearend = 98 + 1
      nTimePnts=cfc_yearend-cfc_yearbeg
      Iunit=28
      OPEN(Iunit,FILE='cfc1112.atm',STATUS='old')
C skip 6 descriptor lines
      DO i =1,6
          READ(Iunit,*)
      ENDDO
C Read in CFC11 and CFC12, N and S Hemisphere time histories
      DO it = 1,nTimePnts
         READ(Iunit,*) year(it),ACFC11(it,1),ACFC12(it,1),
     &        ACFC11(it,2),ACFC12(it,2)
         WRITE(msgBuf,'(A,4F8.2)') 
     &        'year,acfc11_n,acfc12_n,acfc11_s,acfc12_s =',
     &        ACFC11(it,1),ACFC12(it,1),
     &        ACFC11(it,2),ACFC12(it,2)
         CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , 1)

      ENDDO
      _END_MASTER(myThid)
c
#endif



      RETURN
      END
