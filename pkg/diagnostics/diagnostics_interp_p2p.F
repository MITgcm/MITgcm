C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_interp_p2p.F,v 1.1 2006/12/24 20:15:42 jmc Exp $
C $Name:  $
#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INTERP_P2P

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INTERP_P2P(
     O                       qprs,
     I                       qinp,pkz,pksrf,pktop,pk,
     I                       undef, pInc,ijm,lm, myThid )

C     !DESCRIPTION:
C***********************************************************************
C
C PURPOSE
C   To interpolate an arbitrary quantity to Specified Pressure Levels
C
C INPUT
C   QINP .. QINP (ijm,lm) Arbitrary Input Quantity
C   PKZ ... PKZ  (ijm,lm) Pressure to the Kappa at Input Levels
C   PKSRF . PKSRF(ijm) Surface Pressure to the Kappa
C   PKTOP . Pressure to the Kappa at Input-Level-Edge (1) (top of model)
C   PK .... Output Pressure to the Kappa Level (mb)
C   pInc .. if True, assume pressure increases with level index
C   IJM ... Horizontal Dimension of Input
C   LM .... Vertical  Dimension of Input
C
C OUTPUT
C   QPRS .. QPRS (ijm) Arbitrary Quantity at Pressure p
C
C NOTE
C   Quantity is interpolated Linear in P**Kappa.
C   Between  PTOP**Kappa and PKZ(1),  quantity is extrapolated.
C   Between PKSRF**Kappa and PKZ(LM), quantity is extrapolated.
C   Undefined Input quantities are not used.
C   Finally: This routine assumes that pressure levels are counted
C            top down -- ie, level 1 is the top, level lm is the bottom
C
C***********************************************************************
C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      INTEGER  ijm,lm,myThid
      _RL  qinp (ijm,lm)
      _RL  pkz  (ijm,lm)
      _RL  pksrf(ijm)
      _RL  pktop,pk
      _RL  undef
      LOGICAL pInc

C     !OUTPUT PARAMETERS:
      _RL  qprs (ijm)
CEOP

C     !LOCAL VARIABLES:
      INTEGER  i,l
      _RL  pkmin,pkmax,temp

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c Initialize to UNDEFINED
c -----------------------
      DO i=1,ijm
       qprs(i) = undef
      ENDDO

      IF ( pInc ) THEN
C---  Case where Levels are orderd by increasing pressure

c Interpolate to Pressure Between Input Levels
c --------------------------------------------
      DO L=1,lm-1
       pkmin = pkz(1,L)
       pkmax = pkz(1,L+1)

       DO i=2,ijm
         IF ( pkz(i,L)  .LT.pkmin ) pkmin = pkz(i,L)
         IF ( pkz(i,L+1).GT.pkmax ) pkmax = pkz(i,L+1)
       ENDDO

       IF ( pk.LE.pkmax .AND. pk.GE.pkmin ) THEN
         DO i=1,ijm
           IF ( pk.GE.pkz(i,L) .AND. pk.LE.pkz(i,L+1) ) THEN
             temp = ( pk-pkz(i,L) ) / ( pkz(i,L+1)-pkz(i,L) )

             IF     ( qinp(i,L)  .NE.undef  .AND.
     &                qinp(i,L+1).NE.undef ) THEN
               qprs(i) = qinp(i,L+1)*temp + qinp(i,L)*(1.-temp)
             ELSEIF ( qinp(i,L+1).NE.undef  .AND. temp.GE.0.5 ) THEN
               qprs(i) = qinp(i,L+1)
             ELSEIF ( qinp(i,L)  .NE.undef  .AND. temp.LE.0.5 ) THEN
               qprs(i) = qinp(i,L)
             ENDIF
           ENDIF
         ENDDO
       ENDIF

      ENDDO

      DO i=1,ijm
c Extrapolate to Pressure between Ptop and Highest Input Level
c ----------------------------------------------------------
       IF ( pk.LE.pkz(i,1) .AND. pk.GE.pktop ) THEN
         temp = ( pk-pkz(i,1) ) / ( pkz(i,2)-pkz(i,1) )

         IF     ( qinp(i,1).NE.undef  .AND.
     &           qinp(i,2).NE.undef ) THEN
           qprs(i) = qinp(i,2)*temp + qinp(i,1)*(1.-temp)
         ELSEIF ( qinp(i,1).NE.undef ) THEN
           qprs(i) = qinp(i,1)
         ENDIF

       ENDIF

c Extrapolate to Pressure between Psurf and Lowest Input Level
c ------------------------------------------------------------
       IF ( pk.GE.pkz(i,lm) .AND. pk.LE.pksrf(i) ) THEN
         temp = ( pk-pkz(i,lm) ) / ( pkz(i,lm-1)-pkz(i,lm) )

         IF     ( qinp(i,lm)  .NE.undef  .AND.
     &            qinp(i,lm-1).NE.undef ) THEN
            qprs(i) = qinp(i,lm-1)*temp + qinp(i,lm)*(1.-temp)
         ELSEIF ( qinp(i,lm)  .NE.undef ) THEN
            qprs(i) = qinp(i,lm)
         ENDIF

       ENDIF
      ENDDO

      ELSE
C---  Case where Levels are orderd by decreasing pressure

c Interpolate to Pressure Between Input Levels
c --------------------------------------------
      DO L=1,lm-1
       pkmin = pkz(1,L+1)
       pkmax = pkz(1,L)

       DO i=2,ijm
         IF ( pkz(i,L+1).LT.pkmin ) pkmin = pkz(i,L+1)
         IF ( pkz(i,L)  .GT.pkmax ) pkmax = pkz(i,L)
       ENDDO

       IF ( pk.LE.pkmax .AND. pk.GE.pkmin ) THEN
         DO i=1,ijm
           IF ( pk.LE.pkz(i,L) .AND. pk.GE.pkz(i,L+1) ) THEN
             temp = ( pk-pkz(i,L) ) / ( pkz(i,L+1)-pkz(i,L) )

             IF     ( qinp(i,L)  .NE.undef  .AND.
     &                qinp(i,L+1).NE.undef ) THEN
               qprs(i) = qinp(i,L+1)*temp + qinp(i,L)*(1.-temp)
             ELSEIF ( qinp(i,L+1).NE.undef  .AND. temp.GE.0.5 ) THEN
               qprs(i) = qinp(i,L+1)
             ELSEIF ( qinp(i,L)  .NE.undef  .AND. temp.LE.0.5 ) THEN
               qprs(i) = qinp(i,L)
             ENDIF
           ENDIF
         ENDDO
       ENDIF

      ENDDO

      DO i=1,ijm
c Extrapolate to Pressure between Ptop and Highest Input Level
c ----------------------------------------------------------
       IF ( pk.LE.pkz(i,lm) .AND. pk.GE.pktop ) THEN
         temp = ( pk-pkz(i,lm) ) / ( pkz(i,lm-1)-pkz(i,lm) )

         IF     ( qinp(i,lm)  .NE.undef  .AND.
     &            qinp(i,lm-1).NE.undef ) THEN
            qprs(i) = qinp(i,lm-1)*temp + qinp(i,lm)*(1.-temp)
         ELSEIF ( qinp(i,lm)  .NE.undef ) THEN
            qprs(i) = qinp(i,lm)
         ENDIF

       ENDIF

c Extrapolate to Pressure between Psurf and Lowest Input Level
c ------------------------------------------------------------
       IF ( pk.GE.pkz(i,1) .AND. pk.LE.pksrf(i) ) THEN
         temp = ( pk-pkz(i,1) ) / ( pkz(i,2)-pkz(i,1) )

         IF     ( qinp(i,1).NE.undef  .AND.
     &            qinp(i,2).NE.undef ) THEN
           qprs(i) = qinp(i,2)*temp + qinp(i,1)*(1.-temp)
         ELSEIF ( qinp(i,1).NE.undef ) THEN
           qprs(i) = qinp(i,1)
         ENDIF

       ENDIF
      ENDDO

C---  End case increasing/decreasing pressure
      ENDIF

      RETURN
      END
