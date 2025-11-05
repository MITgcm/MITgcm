import numpy as np
import cmocean

yCrossSection = 3000
xCrossSection = 8000
zDepth = -50
plotDPI = 175
usePcolor = False
showZeros = True

# Color Maps
saltCmap = "cmo.haline"
tempCmap = "cmo.thermal"
uCmap = "cmo.balance"
vCmap = "cmo.balance"
wCmap = "cmo.curl"
meltCmap = "cmo.rain"
bergTracerCmap = "cmo.deep"
plumeTracerCmap = "cmo.matter"

# Color Ranges
saltRange = np.linspace(27, 35, 31)
tempRange = np.linspace(-2, 4.0, 31)
uRange = np.linspace(-.2, .2, 31)
vRange = np.linspace(-.2, .2, 31)
wRange = np.linspace(-.005, .005, 31)
meltRange = np.linspace(0,4,31)
bergTracerRange = np.linspace(0,0.05,31)
plumeTracerRange = np.linspace(0,0.05,31)
