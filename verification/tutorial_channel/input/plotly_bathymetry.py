import numpy as np
import plotly.offline as pyo
import plotly.graph_objects as go

eddy_resolving = False

# Horizontal grid
# We will construct two horizontal grids. We will use a coarse resolution
# grid (50 km) and an eddy resolving grid (5 km).

Lx = 1e6
Ly = 2e6


# Coarse resolution grid
nx = 20
ny = 40

x = np.linspace(0.5*Lx/nx, Lx-0.5*Lx/nx, num=nx)
y = np.linspace(0.5*Ly/ny, Ly-0.5*Ly/ny, num=ny)


# Eddy resolving grid
if eddy_resolving:
    nx = 200
    ny = 400

    x = np.linspace(0.5*Lx/nx, Lx-0.5*Lx/nx, num=nx)
    y = np.linspace(0.5*Ly/ny, Ly-0.5*Ly/ny, num=ny)
else:
    pass


# Bathymetry
# Generate a bathymetry file with a wall at the southern boundary and a number of sea mounts.

np.random.seed(42)

bathy = np.zeros((ny, nx), dtype='float64') - 3982.2608621419236
# 3982.2608621419236 comes from summing the vertical grid (see config.py)

X, Y = np.meshgrid(x,y)

N_mountains = 38
Hmax_mountains = 1.5e3
Rmax_mountains = 2e5

x0 = Lx*np.random.uniform(0.25,0.75,N_mountains)
y0 = Ly* np.random.uniform(0,1,N_mountains)
h = Hmax_mountains*np.random.uniform(0,1,N_mountains)
r = Rmax_mountains*np.random.uniform(0,1,N_mountains)

for n in range(N_mountains):
    mountain = h[n]*np.exp(-((X-x0[n])**2 + (Y-y0[n])**2)/r[n]**2 )
#     print(mountain.max())
    bathy = bathy + mountain

# wall at southern boundary
bathy[0,:] = 0


data = [go.Surface(x=X/1e3, y=Y/1e3, z=bathy,
                        colorscale='Viridis',
                        surfacecolor=bathy,
                        colorbar=dict(title='Depth (m)', titlefont={'size':30}, x=1)),
        ]

layout = go.Layout(title='Channel model bathymetry',
                    titlefont=dict(size=40),
                    xaxis = dict(
                        title='X coordinate'),
                    yaxis=dict(title='Y coordinate'),
                    )

fig = go.Figure(data=data, layout=layout)
fig.update_layout(scene_aspectmode='manual',
                  scene_aspectratio=dict(x=1, y=2, z=1))
pyo.plot(fig, filename='bathymetry.html')
