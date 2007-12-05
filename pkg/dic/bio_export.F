C $Header: /u/gcmpack/MITgcm/pkg/dic/bio_export.F,v 1.20 2007/12/05 16:52:32 dfer Exp $
C $Name:  $

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
#ifdef USE_QSW
#include "FFIELDS.h"
#endif

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
      _RL nutlimit
#ifdef AD_SAFE
      _RL thx, thy, theps
#endif
CEOP


#ifndef READ_PAR
#ifndef USE_QSW
        call insol(myTime,sfac,bj)
#endif
#endif
        DO j=jmin,jmax
         DO i=imin,imax
C Fortran-90
CRG C$TAF INIT bio_export = static, nlev
C FORTRAN-77 dynamic memory uses adstore adresto
CRG C$TAF INIT bio_export = memory
C FORTRAN-77 with know max of nlev
C$TAF INIT bio_export = static, 10
#ifdef READ_PAR
           lit=PAR(i,j,bi,bj)
#elif (defined USE_QSW)
           lit=-parfrac*Qsw(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#else
           lit=sfac(j)
#endif

           IF ( .NOT. QSW_underice ) THEN
c if using Qsw but not seaice/thsice or coupled, then
c ice fraction needs to be taken into account
              lit=lit*(1. _d 0 - FIce(i,j,bi,bj))
           ENDIF
c
           DO k=1,nlev
C$TAF STORE lit = bio_export
             atten=(k0*drF(k)*hFacC(i,j,k,bi,bj)*.5 _d 0)
             if (k.gt.1) atten=atten+(k0*drF(k-1)
     &                         *hFacC(i,j,k-1,bi,bj)*.5 _d 0)
             lit=lit*exp(-atten)
             IF (lit.LT.0. _d 0.OR.lit.GT.350. _d 0) THEN
                 print*,'QQ lit', lit
             ENDIF

#ifdef ALLOW_FE
#ifdef AD_SAFE
             thx = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4)
             thy = PTR_FE(i,j,k)/(PTR_FE(i,j,k)+KFE)
             theps = 1. _d -6
             nutlimit= ( 1. _d 0 - tanh((thx-thy)/theps) ) * thx/2. _d 0
     &                +( 1. _d 0 + tanh((thx-thy)/theps) ) * thy/2. _d 0
#else
             nutlimit=   min(PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4),
     &                        PTR_FE(i,j,k)/(PTR_FE(i,j,k)+KFE) )
#endif
#else
             nutlimit=    PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+KPO4)
#endif

             bioac(i,j,k)=alpha(i,j,bi,bj)*
     &                    lit/(lit+lit0)*maskC(i,j,k,bi,bj)*
     &                    nutlimit
            ENDDO
          ENDDO
        ENDDO
c
#endif
#endif
       RETURN
       END
