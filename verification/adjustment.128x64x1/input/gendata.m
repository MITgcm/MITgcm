nx=128;ny=64;					% Grid dimensions

radi=30;					% Radius of patch
deltaPs=100;					% Amplitude

x=(0.5:nx)/nx*360;				%  0  < x < 360
y=(0.5:ny)/ny*180-90;				% -90 < y < +90
[X,Y]=ndgrid(x,y);

h=deltaPs*0.5*(1-cos(max(Y-90+radi,0*Y)/radi*pi));	% Horizontal structure
h=h*deltaPs/max(max(abs(h)));		% Re-scale to give max val

fid=fopen('ps.init','w','b');
fwrite(fid,h,'real*8');
fclose(fid);
