% Script to generate the broken line that folows the cubic grid and
%  stay as close as possible to the great circle arc between 2 given points A,B.
% Transport computed from this broken line (across great circle arc AB) is
%  positive when the flow goes from right to left of arc A -> B.
% (e.g.: if lat_A = lat_B and long_A < long_B, northward flow -> transport > 0 )
%-----------------------
%- kwr=1 : => write broken-line to file: filnam(.mat)
%- kplot : to check different steps of making bk-line:
%  < 0 : no plot ; 0 : just plot cos(longitude) after rotation
%  = 1 : add bk-line after "find_bk_line" ;
%  = 2 : same after "clean_bk_line" ;
%  = 3 : same after "save_bk_line" ;
%  = 4 : same after "shift_bk_line" (= final step);

kwr=0; kplot=4;

%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 rac='grid_files/';
 G=load_grid(rac,10+ncdf);
 xc0=G.xC; yc0=G.yC; xg0=G.xG; yg0=G.yG; arc=G.rAc;
 nc=G.dims(2); ; ncp=nc+1 ;
nXY=6*nc*nc;
nXYz=nXY+2;

xa0=-10;
ya0=-45;
xb0=80;
yb0=45;
filnam='bkl_AB';

%xa0=10;
%ya0=2;
%xb0=30;
%yb0=2;

%----------------------------------
rad=pi/180;

xg0=reshape(xg0,[nXY 1]);
yg0=reshape(yg0,[nXY 1]);
%- add th 2 missing corner:
xg0(nXY+1)=xg0(1); yg0(nXY+1)=yg0(1+2*nc);
xg0(nXY+2)=xg0(1+3*nc); yg0(nXY+2)=yg0(1);

%----------------------------------
dx=(xg0-xa0);
dy=(yg0-ya0);
dr=dx.*dx+dy.*dy;
drMn=min(dr(:)); [I J]=find(dr==drMn);
if length(I) > 1,
 fprintf(' find more than 1 point next to A\n')
else
 if length(I) ==0,
   fprintf(' did not find any point next to A\n')
   return
 end
end
xa=xg0(I(1),J(1));
ya=yg0(I(1),J(1));

dx=(xg0-xb0);
dy=(yg0-yb0);
dr=dx.*dx+dy.*dy;
drMn=min(dr(:)); [I J]=find(dr==drMn);
if length(I) > 1,
 fprintf(' find more than 1 point next to B\n')
else
 if length(I) ==0,
   fprintf(' did not find any point next to B\n')
   return
 end
end
xb=xg0(I(1),J(1));
yb=yg0(I(1),J(1));

fprintf('A point: %8.3f %8.3f\n',xa,ya);
fprintf('B point: %8.3f %8.3f\n',xb,yb);
%----

[xcg,ycg,xcs,ycs,alp,bet]=rotate_xy(xg0,yg0,xc0,yc0,xa,ya,xb,yb);

%-----------------

xx1=split_Z_cub(xcg);
yy2=split_Z_cub(ycg);
%-- define dylat = width of Lat. band that contains the broken line:
ddy=zeros(ncp,ncp,6);
ddy(1:nc,:,:)=yy2(1:nc,:,:)-yy2(2:ncp,:,:); ddy=abs(ddy);
dyImx=max(ddy(:)) ;
ddy=zeros(ncp,ncp,6);
ddy(:,1:nc,:)=yy2(:,1:nc,:)-yy2(:,2:ncp,:); ddy=abs(ddy);
dyJmx=max(ddy(:));
dylat=sqrt(2)*max(dyImx,dyJmx);
fprintf('dyImx,dyJmx = %7.3f %7.3f ; ==> select dylat= %9.6f \n', ...
         dyImx,dyJmx,dylat);

%-----------------
% to avoid -180,+180 jump in long, center longitude arround xMid:
xMid=zeros(6,1);
for n=1:6,
  var=reshape(yy2(:,:,n),[ncp*ncp 1]);
  [I]=find(abs(var) <= dylat);
  fprintf(' face: %i, %i points in dylat band',n,length(I));
 if length(I) > 0,
  var=reshape(xx1(:,:,n),[ncp*ncp 1]);
  xx=var(I)*rad; xM=mean(cos(xx)); yM=mean(sin(xx));
  xMid(n)=atan2(yM,xM)/rad;
  fprintf(', xMid= %8.3f\n',xMid(n));
 end
end
xMid=round(xMid);
xx2=xx1;
for n=1:6,
 if xMid(n) ~= 0,
   dec=xMid(n)-180;
   xx2(:,:,n)=rem( xx2(:,:,n)+360-dec, 360) +dec ;
 end
 fprintf('face  %i : xMid= %8.2f ; Xmin,Xmax = %8.3f %8.3f \n', ...
         n, xMid(n), min(min(xx2(:,:,n))), max(max(xx2(:,:,n))) );
end

yl=0; nf1=1; nf2=6; ydim=1; jl=1;
XYout=1000;
%nMx6t=zeros(6,1);
%savI=zeros(nc*6,6); savJ=zeros(nc*6,6); savF=zeros(nc*6,6);
%isav=zeros(nc*6,6); jsav=zeros(nc*6,6); xsav=zeros(nc*6,6);
%find_bk_line
[savI,savJ,savF,isav,jsav,xsav,nMx6t] = ...
find_bk_line( nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx1,xx2,yy2,xcs,ycs );

%- define "segments" = continuous part of the broken line :
%ncut=zeros(6,1); icut=zeros(nc,6,6); xcut=zeros(nc,4,6); ycut=zeros(nc,4,6);
%clean_bk_line
[ncut,icut,xcut,ycut,misfit,xyfit] = ...
clean_bk_line( nf1,nf2,nc,ydim,yl,dylat,xMid,xx1,xx2,yy2, ...
               savI,savJ,savF,isav,jsav,xsav,nMx6t );

if misfit > 0,
 fprintf('misfit= %i , xyfit= %i ; ==> must do something ! \n', ...
    misfit,xyfit);
 return
end

%-----------------

xx0=split_Z_cub(xg0);
yy0=split_Z_cub(yg0);

%- output : put together the pieces of bk-lines from the 6 faces :
%save_bk_line
savNpts=zeros(ydim,1);
savFlg=zeros(6*ncp,ydim);
savIuv=zeros(6*ncp,ydim); savJuv=zeros(6*ncp,ydim);
savXsg=zeros(6*ncp,ydim); savYsg=zeros(6*ncp,ydim);

[svNpts,svFlg,svIuv,svJuv,svXsg,svYsg,svXx1,svYy1]= ...
save_bk_line( nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx0,yy0,yy2, ...
              savI,savJ,savF,isav,jsav,xsav,ncut,icut,xcut,ycut );
%- easier to debug this way:
savNpts(jl)=svNpts;
savFlg(:,jl)=svFlg;
savIuv(:,jl)=svIuv;
savJuv(:,jl)=svJuv;
savXsg(:,jl)=svXsg;
savYsg(:,jl)=svYsg;

[ab_Npts,ab_Flg,ab_IJuv,ab_Xsg,ab_Ysg] = ...
shift_bk_line( nc,ydim,jl,xa,ya,xb,yb,savNpts,savFlg,savIuv,savJuv,savXsg,savYsg );

if kwr == 1,
%- save to matlab file "filnam":
filnam='bkl_AB';
 namf=[filnam,'_cs',int2str(nc)];
 fprintf(['save bk-line to file: ',namf,'\n']);
 save(namf,'ab_Npts','ab_Flg','ab_IJuv','ab_Xsg','ab_Ysg');
end
%-----------------
if kplot < 0 , return ; end

ccB=[0 0]; shift=-1; cbV=1; AxBx=[-180 180 -90 90];
%AxBx=[-50 30 -50 -10];
%var=ycs;
var=cos(xcs*rad); ccB=[-1 1]*3;
figure(1);clf;
grph_CS(var,xc0,yc0,xg0,yg0,ccB(1),ccB(2),shift,cbV,AxBx);
hold on;
[L]=line([xa xb],[ya yb]); set(L,'Color',[1 1 1]);
plot(alp/rad,bet/rad,'ro-');
%nj=21; plot(xx0(isav(nj,1),jsav(nj,1),1)+1,yy0(isav(nj,1),jsav(nj,1),1)+1,'cx');
if kplot == 1,
 for n=nf1:nf2, if nMx6t(n) > 0,
  xloc=zeros(nMx6t(n),1); yloc=xloc;
  for i=1:nMx6t(n),
   xloc(i)=xx0(isav(i,n),jsav(i,n),n);
   yloc(i)=yy0(isav(i,n),jsav(i,n),n);
  end
  plot(xloc,yloc,'g*-');
 end ; end
end
if kplot == 2,
 clrs='bscdmvr^y<g>kp';
 clear P ; np=0;
 for n=nf1:nf2, if ncut(n) > 0,
  xloc=zeros(6*nc,1); yloc=zeros(6*nc,1);
  for i=1:nMx6t(n),
   xloc(i)=xx0(isav(i,n),jsav(i,n),n);
   yloc(i)=yy0(isav(i,n),jsav(i,n),n);
  end
  for in=1:ncut(n), if icut(in,6,n) >= 0,
    is=3; if icut(in,is,n) <= 0, is=1 ; end
    ie=4; if icut(in,ie,n) <= 0, ie=2 ; end
    if icut(in,is,n) > icut(in,ie,n), is=1 ; ie=2 ; end
    if icut(in,is,n) > 0 & icut(in,ie,n) > 0 & icut(in,is,n) < icut(in,ie,n),
      np=np+1;
%     P(np)=plot(xsav(icut(in,is,n):icut(in,ie,n),n), ...
      P(np)=plot(xloc(icut(in,is,n):icut(in,ie,n)), ...
                 yloc(icut(in,is,n):icut(in,ie,n)),[clrs(1+2*(np-1):2*np),'-']);
 %               yloc(icut(in,is,n):icut(in,ie,n)),'g*-');
    end
  end; end
 end; end
 set(P,'LineWidth',4);
end
if kplot == 3,
 clear P ; np=1; ict(1)=0;
 sNp=savNpts(jl)+1;
 xloc=savXsg(1:sNp,jl);
 yloc=savYsg(1:sNp,jl);
%xloc=ab_Xsg; yloc=ab_Ysg;
 dd=abs(xloc(2:sNp)-xloc(1:sNp-1));
 fprintf(' np, Max(dd) : %i %8.3f\n',np,max(dd(:)) );
 while max(dd(:)) > 2*dylat,
  [I]=find(dd==max(dd(:)));
  ict(np+1)=I;
  xloc(I+1:sNp+1)=xloc(I:sNp);
  yloc(I+1:sNp+1)=yloc(I:sNp);
  sNp=sNp+1; np=np+1;
  dec=xloc(I+2)-180;
  xloc(I+1)=dec+rem(xloc(I+1)-dec+360,360);
  for j=1:np-1, if ict(j) > I, ict(j)=ict(j)+1; end; end
  dd=abs(xloc(2:sNp)-xloc(1:sNp-1));
  for j=2:np, dd(ict(j))=0; end
  fprintf(' np, Max(dd) : %i %8.3f\n',np,max(dd(:)) );
 end
 ict(np+1)=sNp;
 for j=1:np,
   P(j)=plot(xloc(1+ict(j):ict(j+1)),yloc(1+ict(j):ict(j+1)),'r*-');
 end
 set(P,'LineWidth',4);
end
if kplot == 4,
 clear P ; sNp=ab_Npts+1;
 xloc=ab_Xsg(1:sNp);
 yloc=ab_Ysg(1:sNp);
 P=plot(xloc,yloc,'b*-');
 set(P,'LineWidth',4);
end
hold off
title('COS( new-long )');
