% This is a matlab script that generates the input data

nx=32; ny=nx; nf=6;
kwr=1;

%- flat bottom bathy with rectangular island on face 2
dp=ones(nx,nf,ny);
Htot=1366;
dp=-Htot*dp;

fc=2;
i1=1; i2=nx; j1=ny/4 ; j2=1+3*ny/4;
dp(:,fc,j1:j2)=0;
dp(nx,1,j1:j2)=0;
dp(j1:j2,4,1)=0;

if kwr > 0,
 fid=fopen('bathy_f2.bin','w','b');
 fwrite(fid,dp,'real*8');
 fclose(fid);
end

%----------

h=zeros(nx,nf,ny);

x=0.25*((0.5:nx)/nx-0.5);
[X,Y]=ndgrid(x,x);
R=sqrt(X.^2+Y.^2);


tileno=1;

o=1;
i=round(nx*1/2);
j=round(ny*1/2);
h(i:i+o,tileno,j:j+o)=1;

h(:,1,:)=0.5+0.5*cos(   pi*min(R,0*R+0.04)/0.04 );

%- 10 m max SSH anomaly:
h=h*10;

if kwr > 0,
 fid=fopen('ssh_eq.bin','w','b');
 fwrite(fid,h,'real*8');
 fclose(fid);
end

return
%----------
% plot to check : ----------
rDir='/home/jmc/grid_cs32/';
G=load_grid(rDir,10);
nc=G.dims(2); %nr=G.dims(3); nPxy=G.dims(1)*G.dims(2); nPp2=nPxy+2;
ccB=[0 0]; shift=-1; cbV=0; AxBx=[-180 180 -90 90]; kEnv=0;

figure(1);clf;
var=reshape(dp,[nx*nf ny]);
grph_CS(var,G.xC,G.yC,G.xG,G.yG,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);

figure(2);clf;
var=reshape(h,[nx*nf ny]);
grph_CS(var,G.xC,G.yC,G.xG,G.yG,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);

%----------
