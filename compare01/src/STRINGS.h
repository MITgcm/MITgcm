C $Id: STRINGS.h,v 1.1 1998/05/25 20:21:07 cnh Exp $
C Definition of symbols used to represent strings in the model code.
C
C     topBoxPhZero - Sets hydrostatic pressure boundary condition.
C     topBoxPhIntegralOfRho - Alternate hydrostatic pressure boundary condition.
C     cartesian    - Identifies gridding style.
C     sphericalPolar - Identifies alternate gridding style.
C     linear       - Identifies equation os state.
C     polynomial   - Identifies equation of state
C     poly3dFile   - Name of file holding coefficients for polynomial state eqn.
C     referenceAbove - Instruction to reference density to layer above.
C     referenceLocal - Instruction to reference density to local depth.
C     uForcingfile   - Name of file holding u forcing field(s).
C     vForcingfile   - Name of file holding v forcing field(s).
C     fPlane         - Identifies coriolis variation.
C     betaPlane      - Identifies coriolis variation. 
C     commentCharacter - Character which delimits comments in data file.
      CHARACTER*(*) topBoxPhZero
      CHARACTER*(*) topBoxPhIntegralOfRho
      CHARACTER*(*) cartesian
      CHARACTER*(*) sphericalPolar
      CHARACTER*(*) linear
      CHARACTER*(*) polynomial
      CHARACTER*(*) poly3dFile
      CHARACTER*(*) referenceAbove
      CHARACTER*(*) referenceLocal
      CHARACTER*(*) uForcingFile
      CHARACTER*(*) vForcingFile
      CHARACTER*(*) fPlane
      CHARACTER*(*) betaPlane
      CHARACTER*(*) commentCharacter
      CHARACTER*(*) binaryBathymetry
      CHARACTER*(*) textBathymetry
      CHARACTER*(*) inlineBathymetry
      PARAMETER    (topBoxPhZero =          'ZERO' )
      PARAMETER    (topBoxPhIntegralOfRho = 'RHOTOP' )
      PARAMETER    (cartesian =             'CARTESIAN' )
      PARAMETER    (sphericalPolar =        'SPHERICAL POLAR')
      PARAMETER    (linear =                'LINEAR' )
      PARAMETER    (polynomial =            'POLYNOMIAL' )
      PARAMETER    (poly3dFile =            'poly3d.coeffs')
      PARAMETER    (referenceAbove =        'RHO ABOVE' )
      PARAMETER    (referenceLocal =        'RHO LOCAL' )
      PARAMETER    (uforcingFile  =         'windx' )
      PARAMETER    (vforcingFile  =         'windy' )
      PARAMETER    (fPlane =                'F PLANE')
      PARAMETER    (betaPlane =             'BETA PLANE')
      PARAMETER    (commentCharacter =      '#'         )
      PARAMETER    (binaryBathymetry =      'BINARY'    )
      PARAMETER    (textBathymetry   =      'TEXT'      )
      PARAMETER    (inlineBathymetry =      'INLINE'    )
