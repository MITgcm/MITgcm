#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE BIO_EXPORT( PTR_PO4 , 
#ifdef ALLOW_FE
     I           PTR_FE, 
#endif   
     I           bioac,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE BIO_EXPORT                                     |
C     | o Calculate biological activity and export                |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_ABIOTIC.h"
#include "DIC_BIOTIC.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_FE
      _RL  PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      _RL bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR)
      _RL sfac(1-OLy:sNy+OLy)
      _RL lit, atten
      INTEGER imin, imax, jmin, jmax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC
C     == Local variables ==
       INTEGER I,J,k
c
        call insol(myTime,sfac,bj)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           lit=sfac(j)
           DO k=1,nlev
             atten=(k0*delz(k)*.5)
             if (k.gt.1) atten=(k0*delz(k-1)*.5)
             lit=lit*exp(-atten)*(1.d0-Fice(i,j,bi,bj))
             if (lit.lt.0.d0.or.lit.gt.150) then
                 print*,'QQ lit', lit
             endif
             bioac(i,j,k)=alpha(i,j,bi,bj)*
     &                    PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4)*
#ifdef ALLOW_FE
     &                    PTR_FE(i,j,k)/(PTR_FE(i,j,k)+KFE)*
#endif
     &                    lit/(lit+lit0)*maskC(i,j,k,bi,bj)
            ENDDO
          ENDDO
        ENDDO
c
#endif
#endif
       RETURN
       END
