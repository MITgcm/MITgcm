#ifdef ALLOW_THSICE
C     *==========================================================*
C     | THSICE_SIZE.h Declare size of arrays for Therm_SeaIce pkg
C     *==========================================================*

C.. number layers of ice
C     nlyr   ::   maximum number of ice layers
      INTEGER nlyr
      PARAMETER (nlyr = 2)

C--   Energy distribution (lateral / thickening-thinning) using a power law:
C     power-law exponent is set to: 1+2^powerLawExp2
      INTEGER     powerLawExp2
      PARAMETER ( powerLawExp2 = 2 )

C--   identifiers for advected properties
      INTEGER GAD_SI_FRAC, GAD_SI_HSNOW
      INTEGER GAD_SI_HICE, GAD_SI_QICE1, GAD_SI_QICE2
      PARAMETER ( GAD_SI_FRAC  = -5,
     &            GAD_SI_HSNOW = -6,
     &            GAD_SI_HICE  = -7,
     &            GAD_SI_QICE1 = -8,
     &            GAD_SI_QICE2 = -9 )

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER MaxTsf
      PARAMETER ( MaxTsf = 20 )
C     The number of properties to be advected is needed to define static tapes.
      INTEGER thSIce_nAdv
c     PARAMETER ( thSIce_nAdv = 1 + GAD_SI_FRAC - GAD_SI_QICE2 )
      PARAMETER ( thSIce_nAdv = 5 )
#endif

#endif /* ALLOW_THSICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
