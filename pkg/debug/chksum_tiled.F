C $Header: /u/gcmpack/MITgcm/pkg/debug/chksum_tiled.F,v 1.4 2003/10/09 04:19:19 edhill Exp $
C $Name:  $

#include "DEBUG_OPTIONS.h"

      SUBROUTINE CHKSUM_TILED( str,fld,NN,bi,bj,myThid )
      IMPLICIT NONE
C     Global/common
#include "SIZE.h"
#include "EEPARAMS.h"
C     Routines arguments
      CHARACTER*(*) str
      INTEGER NN,bi,bj,myThid
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NN,nSx,nSy)
C     Local
      INTEGER I,J,K
      INTEGER cnt
      _RL Fmn,Fmin,Fmax,Frms

      Fmn=0.
      Frms=0.
      Fmin=fld(1,1,1,bi,bj)
      Fmax=fld(1,1,1,bi,bj)
      cnt=0
      DO K=1,NN
       DO J=1,sNy
        DO I=1,sNx
         Fmin=min(Fmin,fld(I,J,K,bi,bj))
         Fmax=max(Fmax,fld(I,J,K,bi,bj))
         Fmn=Fmn+fld(I,J,K,bi,bj)
         cnt=cnt+1
        ENDDO
       ENDDO
      ENDDO
      Fmn=Fmn/float(cnt)
      DO K=1,NN
       DO J=1,sNy
        DO I=1,sNx
         Frms=Frms+(fld(I,J,K,bi,bj)-Fmn)**2
        ENDDO
       ENDDO
      ENDDO
      Frms=sqrt(Frms/float(cnt))

      write(*,'(a,1p4e12.5,1x,a)')
     &      'CHKSUM_TILED: ',Fmin,Fmax,Fmn,Frms,str

      RETURN
      END
