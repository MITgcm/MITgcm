#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: BIO_EXPORT

C !INTERFACE: ==========================================================
      SUBROUTINE BIO_EXPORT( PTR_PO4 , 
#ifdef ALLOW_FE
     I           PTR_FE, 
#endif   
     I           bioac,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

c !DESCRIPTION:
C  Calculate biological activity and export                

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_ABIOTIC.h"
#include "DIC_BIOTIC.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  PTR_PO4              :: phosphate tracer field
C  PTR_FE               :: iron tracer field
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_FE
      _RL  PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER imin, imax, jmin, jmax, bi, bj


C !OUTPUT PARAMETERS: ==================================================
C  bioac               :: biological productivity (will be split
C                         between export and dissolved pool)
      _RL bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR)

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
       INTEGER I,J,k
      _RL sfac(1-OLy:sNy+OLy)
      _RL lit, atten
CEOP
#ifndef READ_PAR
        call insol(myTime,sfac,bj)
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C Fortran-90
CRG C$TAF INIT bio_export = static, nlev
C FORTRAN-77 dynamic memory uses adstore adresto
CRG C$TAF INIT bio_export = memory
C FORTRAN-77 with know max of nlev
C$TAF INIT bio_export = static, 10
#ifdef READ_PAR
           lit=PAR(i,j,bi,bj)
#else
           lit=sfac(j)
#endif
           DO k=1,nlev
C$TAF STORE lit = bio_export
             atten=(k0*drF(k)*hFacC(i,j,k,bi,bj)*.5)
             if (k.gt.1) atten=atten+(k0*drF(k-1)
     &                         *hFacC(i,j,k-1,bi,bj)*.5)
             lit=lit*exp(-atten)*(1.d0-Fice(i,j,bi,bj))
             if (lit.lt.0.d0.or.lit.gt.150) then
                 print*,'QQ lit', lit
             endif
             bioac(i,j,k)=alpha(i,j,bi,bj)*
     &                    lit/(lit+lit0)*maskC(i,j,k,bi,bj)*
#ifdef ALLOW_FE
     &                    min(PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4),
     &                        PTR_FE(i,j,k)/(PTR_FE(i,j,k)+KFE) ) 
#else
     &                    PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4)
#endif
            ENDDO
          ENDDO
        ENDDO
c
#endif
#endif
       RETURN
       END
