C $Header: /u/gcmpack/MITgcm/pkg/autodiff/copy_advar_outp.F,v 1.3 2014/04/04 23:06:04 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#include "AD_CONFIG.h"

CBOP
C !ROUTINE: COPY_ADVAR_OUTP
C !INTERFACE:
      SUBROUTINE COPY_ADVAR_OUTP(
     I                            inpFldRS, inpFldRL,
     O                            outFld,
     I                            nNz, vType, myThid )

C !DESCRIPTION:
C     Copy input AD-variable (RS or RL) into output array and then, according
C      to variable type, apply ADEXCH to output array.
C     vType (1rst digit):
C           = 1,3 : process RS input field ; = 2,4 : process RL input field
C           = 1,2 : without sign. ;          = 3,4 : with sign.
C     vType (2nd digit) = 10 : A-grid location (i.e., grid-cell center)
C                       = 20 : B-grid location (i.e., grid-cell corner)

C     !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C Routine arguments
C inpFldRS ( RS )  :: input AD-variable field
C inpFldRL ( RL )  :: input AD-variable field
C outFld   ( RL )  :: copy of input field
C nNz     (integer):: third dimension of 3-D input/output field
C vType   (integer):: type of AD-variable (select which ADEXCH to use)
C myThid  (integer):: my Thread Id number
      INTEGER nNz
      _RS    inpFldRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL    inpFldRL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL    outFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nNz,nSx,nSy)
      INTEGER vType
      INTEGER myThid

#ifdef ALLOW_AUTODIFF_MONITOR
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
C !LOCAL VARIABLES:
C   i,j,k   :: loop indices
C   bi,bj   :: tile indices
C   gridloc :: advar horizontal-grid location
      INTEGER i,j,k,bi,bj
      INTEGER gridloc
      LOGICAL wSign
CEOP

      gridloc = vType/10
      IF ( MOD(vType,10).LT.1 .OR. MOD(vType,10).GT.4
     &      .OR. gridloc.LT.1 .OR. gridloc.GT.2 ) THEN
        STOP 'ABNORMAL END: COPY_ADVAR_OUTP invalid vType'
      ENDIF
      wSign = MOD(vType,10).GE.3

      IF ( MOD(vType,2).EQ.1 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,nNz
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             outFld(i,j,k,bi,bj) = inpFldRS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,nNz
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             outFld(i,j,k,bi,bj) = inpFldRL(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_OPENAD
C--   need to all the correct OpenAD EXCH S/R ; left empty for now
#else /* ALLOW_OPENAD */

      IF ( gridloc.EQ.1 ) THEN
       IF ( wSign ) THEN
#ifdef AUTODIFF_TAMC_COMPATIBILITY
c       CALL ADEXCH_SM_3D_RL( wSign, nNz, myThid, outFld )
#else
c       CALL ADEXCH_SM_3D_RL( outFld, wSign, nNz, myThid )
#endif
        STOP 'ABNORMAL END: COPY_ADVAR_OUTP missing wSign,loc=1'
       ELSE
#ifdef AUTODIFF_TAMC_COMPATIBILITY
        CALL ADEXCH_3D_RL( nNz, myThid, outFld )
#else
        CALL ADEXCH_3D_RL( outFld, nNz, myThid )
#endif
       ENDIF
      ELSEIF ( gridloc.EQ.2 ) THEN
       IF ( wSign ) THEN
        STOP 'ABNORMAL END: COPY_ADVAR_OUTP wSign,loc=2 not coded'
       ELSE
#ifdef AUTODIFF_TAMC_COMPATIBILITY
c       CALL ADEXCH_Z_3D_RL( nNz, myThid, outFld )
#else
c       CALL ADEXCH_Z_3D_RL( outFld, nNz, myThid )
#endif
        STOP 'ABNORMAL END: COPY_ADVAR_OUTP missing noSign,loc=2'
       ENDIF
      ENDIF
#endif /* ALLOW_OPENAD */

#endif /* ALLOW_ADJOINT_RUN or ALLOW_ADMTLM */
#endif /* ALLOW_AUTODIFF_MONITOR */
      RETURN
      END
