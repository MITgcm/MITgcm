setMinDepth=input('setMinDepth (>0) = ? ');

h0=rdda('bathy_cs32.bin',[6*32 32],1,'real*8','b');
h1=min(0,h0);
[I J]=find(-setMinDepth<h1 & h1<0 );
fprintf('make deeper %i grid points\n',length(I));
h1(find(-setMinDepth<h1 & h1<0 ))=-setMinDepth;

namfil=['bathy_Hmin',int2str(setMinDepth),'.bin'];
fid=fopen(namfil,'w','b'); fwrite(fid,h1,'real*8'); fclose(fid);
fprintf([' write bathy on file:',namfil,' \n']);

return

rac='grid_cs32/';
set_axis 
load_cs
var=min(h1,0); var(find(var==0))=NaN;
shift=-1; cbV=0; AxBx=[-180 180 -90 90];
ccB=[-5000 100];
grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);

