C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/atparam0.h,v 1.3 2001/02/04 14:38:49 cnh Exp $
C $Name:  $
C $Namer: $

      INTEGER ISC
      PARAMETER (ISC=1)

      INTEGER NTRUN, MTRUN, IX
      PARAMETER ( NTRUN=21, MTRUN=21, IX=sNx )   

      INTEGER MX, MX2, IL, NTRUN1, MXP
      PARAMETER (MX=MTRUN+1 , MX2=2*MX)
      PARAMETER (IL=sNy, NTRUN1=NTRUN+1 )
      PARAMETER ( MXP=ISC*MTRUN+1 )
