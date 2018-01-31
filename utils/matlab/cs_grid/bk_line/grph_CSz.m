function grph_CSz(var,xcs,ycs,xcg,ycg,c1,c2,shift,cbV,AxBx,kEnv)
% grph_CSz(var,xcs,ycs,xcg,ycg,c1,c2,shift[,cbV,AxBx,kEnv]) :
% produce a flat plot of the cube_sphere "var" (@ C-grid corner position)
%  keeping the initial grid (no interpolation, use "surf").
% xcs,ycs,xcg,ycg = center + corner grid point coordinates
% c1 < c2 = min & max for the color graph
% c1 > c2 = scale with min,max of the field, + c1/100 and + c2/100
% shift=-1 : No coast-line
% shift= 1 : No shift but draw coast-line calling draw_coast
%     else : draw coast-line (using m_proj) shifted by "shift" degree.
% cbV = 0,1 : horizontal,vertical colorbar; >= 2 : no colorbar;
% kEnv = 0 : standard ; =odd : do not draw the mesh ; >1 : no min,Max written.
% AxBx = do axis(AxBx) to zoom in Box "AxBx" ; only if shift=-1 ;
%-----------------------
% Written by jmc@ocean.mit.edu, 2005.
%- small number (relative to lon,lat in degree)
epsil=1.e-6;
%- mid-longitude of the grid (truncated @ epsil level):
xMid=mean(xcs(:)); xMid=epsil*round(xMid/epsil);
%fprintf(' mid Longitude of the grid: %22.16e\n',xMid);

if nargin < 9, cbV=0 ; end
if nargin < 10, AxBx=[xMid-180 xMid+180 -90 90] ; end
if nargin < 11, kEnv=0 ; end

%------------------------------
ncx=size(xcs,1); nc=size(xcs,2) ; ncp=nc+1 ; nPt2=size(var,1);
nPts=ncx*nc;
%- check dim
 if ncx ~= 6*nc | nPt2 ~= nPts+2,
  fprintf('Bad dim (input fields): nc,ncx,nPts,nPt2= %i %i %i %i\n',nc,ncx,nPts,nPt2);
  error(' wrong dimensions ');
 end
 mnV=min(var);
 MxV=max(var);
% mx=min(min(var));
% mn=max(max(var));
%if shift == 1,
% for j=1:nc, for i=1:6*nc,
%  if var(i,j) ~= NaN & xcs(i,j) > AxBx(1) & xcs(i,j) < AxBx(2) ...
%                     & ycs(i,j) > AxBx(3) & ycs(i,j) < AxBx(4) ,
%     mn=min(var(i,j),mn); mx=max(var(i,j),mx) ; end
% end ; end ;
%else
% for j=1:nc, for i=1:6*nc,
%  if var(i,j) ~= NaN ; mn=min(var(i,j),mn); mx=max(var(i,j),mx) ; end
% end ; end ;
%end
%------------
if c1 >= c2
  mb=(MxV-mnV)*0.01;
  c1=mnV+mb*c1;
  c2=MxV+mb*c2;
  if c1*c2 < 0
   c2=max(abs([c1 c2]));
   c1=-c2;
  end
  fprintf('min,max %8.3e %8.3e Cmin,max %8.3e %8.3e \n',mnV,MxV,c1,c2)
end
%------------------------------
%figure(1);
if shift ~= 1 & shift ~= -1,
 %pphh=path; ppaa='/u/u2/jmc/MATLAB';
  pphh=path; ppaa='/home/jmc/matlab/MATLAB';
  llaa=length(ppaa); if pphh(1:llaa) == ppaa, else path(ppaa,path);end
  set_axis
  fac=rad ;
else
  fac=1. ;
end
%---
nbsf = 0 ; ic = 0 ; jc = 0 ;
vv0=permute(reshape(var(1:nPts,1),[nc 6 nc]),[1 3 2]);
[xx2]=split_C_cub(xcs,1);
[yy2]=split_C_cub(ycs,1);
%---
for n=1:6,
%if n > 2 & n < 4,
 if n < 7,
%--------------------------------------------------------
 i0=nc*(n-1);
 vv1=zeros(ncp,ncp) ; xx1=vv1 ; yy1=vv1 ;
 vv1(1:nc,1:nc)=vv0(:,:,n);
%-----
  xx1=xx2(:,:,n);
  yy1=yy2(:,:,n);
% if xx1(ncp,1) < xMid-300. ; xx1(ncp,1)=xx1(ncp,1)+360. ; end
% if xx1(1,ncp) < xMid-300. ; xx1(1,ncp)=xx1(1,ncp)+360. ; end
%------------
if shift <= -360
%--- Jump ? (only for debug diagnostic) :
 for i=1:nc, for j=1:nc,
   if abs(xx1(i,j)-xx1(i,j+1)) > 120
     fprintf('N: i,J,xx_j,j+1,+C %3i %3i %3i %8.3e %8.3e %8.3e \n', ...
             n, i,j,xx1(i,j), xx1(i,j+1), xcs(i0+i,j) ) ; end
   if abs(xx1(i,j)-xx1(i+1,j)) > 120
     fprintf('N: I,j,xx_i,i+1,+C %3i %3i %3i %8.3e %8.3e %8.3e \n', ...
             n, i,j,xx1(i,j), xx1(i+1,j), xcs(i0+i,j) ) ; end
 end ; end
%---
end
%--------------------------------------
 if n == 4 | n == 3
  jc=2+nc/2 ;
%-- case where Xc jump from < 180 to > -180 when j goes from jc to jc+1 :
%   cut the face in 2 parts (1: x > 0 ; 2: x < 0 ) and plot separately
  xxSav=xx1(:,jc);
  [I]=find(xx1(:,jc) < xMid-120); xx1(I,jc)=xMid+180.;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,1,jc,c1,c2) ;
%-
  xx1(:,jc)=xxSav; jc=jc-1;
  [I]=find(xx1(:,jc) > xMid+120); xx1(I,jc)=xMid-180.;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,jc,ncp,c1,c2) ;
% Note: later on, will plot separately N & S poles with the 2 missing corners.
%---
 elseif n == 6
  ic=1+nc/2 ;
%-- case where Xc jump from < -180 to > 180 when i goes from ic to ic+1 :
%   cut the face in 2 parts (1: x > 0 ; 2: x < 0 ) and plot separately
  xxSav=xx1(ic,:);
  [J]=find(xx1(ic,:) < xMid-120); xx1(ic,J)=xMid+180.;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,ic,ncp,1,ncp,c1,c2) ;
%-
  xx1(ic,:)=xxSav; ic=ic+1;
  [J]=find(xx1(ic,:) > xMid+120); xx1(ic,J)=xMid-180.;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ic,1,ncp,c1,c2) ;
%---
 else
%-- plot the face in 1 piece :
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,1,ncp,c1,c2) ;
 end
%--------------------------------------
end ; end
%--------------
%- add isolated point:
 vvI=zeros(2,2); xxI=vvI; yyI=vvI;

%- 1rst missing corner (N.W corner of 1rst face): nPts+1
 vvI(1,1)=var(nPts+1);
 for l=0:2,
  xxI(1+rem(l,2),1+fix(l/2))=xx2(2,ncp,1+2*l);
  yyI(1+rem(l,2),1+fix(l/2))=yy2(2,ncp,1+2*l);
 end
 xxI(2,2)=xxI(1,2); yyI(2,2)=yyI(1,2);
 [nbsf,S(nbsf)]=part_surf(nbsf,fac,xxI,yyI,vvI,1,2,1,2,c1,c2) ;

%- 2nd missing corner (S.E corner of 2nd face): nPts+2
 vvI(1,1)=var(nPts+2);
 for l=0:2,
  xxI(1+rem(l,2),1+fix(l/2))=xx2(ncp,2,2+2*l);
  yyI(1+rem(l,2),1+fix(l/2))=yy2(ncp,2,2+2*l);
 end
 xxI(2,2)=xxI(1,2); yyI(2,2)=yyI(1,2);
 [nbsf,S(nbsf)]=part_surf(nbsf,fac,xxI,yyI,vvI,1,2,1,2,c1,c2) ;

%- N pole:
 vvI(1,1)=var(1+nc/2+2*nc+nc/2*ncx);
 xxI(1,:)=xMid-180; xxI(2,:)=xMid+180;
 yyI(:,1)=max(ycs(:)); yyI(:,2)=+90;
 [nbsf,S(nbsf)]=part_surf(nbsf,fac,xxI,yyI,vvI,1,2,1,2,c1,c2) ;
%- S pole:
 vvI(1,1)=var(1+nc/2+5*nc+nc/2*ncx);
 xxI(1,:)=xMid-180; xxI(2,:)=xMid+180;
 yyI(:,2)=min(ycs(:)); yyI(:,1)=-90;
 [nbsf,S(nbsf)]=part_surf(nbsf,fac,xxI,yyI,vvI,1,2,1,2,c1,c2) ;
%--------------
 set(S,'LineStyle','-','LineWidth',0.01);
 if rem(kEnv,2) > 0, set(S,'EdgeColor','none'); end
hold off
if shift == -1,
  axis(AxBx); fprintf('  Axis(Box): %i %i %i %i \n',AxBx);
elseif shift == 1
  [L]=draw_coast(fac);
  set(L,'color',[1 0 1]);
% set(L,'Color',[0 0 0],'LineWidth',2); % set(L,'LineStyle','-');
  axis(AxBx); fprintf('  Axis(Box): %i %i %i %i \n',AxBx);
else
  m_proj('Equidistant Cylindrical','lat',90,'lon',[-180+shift 180+shift])
  %m_proj('Equidistant Cylindrical','lat',[-0 60],'lon',[-180+shift 180+shift])
  m_coast('color',[0 0 0]);
  m_grid('box','on')
end

%--
 if cbV < 2, scalHV_colbar([10-cbV/2 10 7-5*cbV 7+2*cbV]/10,cbV); end
if mnV < MxV & kEnv < 2,
 ytxt=min(1,cbV);
 if shift == 1 | shift == -1,
  xtxt=mean(AxBx(1:2)) ; ytxt=AxBx(3)-(AxBx(4)-AxBx(3))*(10+1*ytxt)/100;
  text(xtxt*fac,ytxt*fac,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
  %set(gca,'position',[-.1 0.2 0.8 0.75]); % xmin,ymin,xmax,ymax in [0,1]
 else text(0,(30*cbV-120)*fac,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
 end
else fprintf('Uniform field: min,Max= %g \n',MxV); end
return
%----------------------------------------------------------------------
function [nbsf,S]=part_surf(nbsf,fac,xx,yy,vv,i1,i2,j1,j2,c1,c2)
  S=surf(fac*xx(i1:i2,j1:j2),fac*yy(i1:i2,j1:j2), ...
         zeros(i2+1-i1,j2+1-j1),vv(i1:i2,j1:j2)) ;
  if c1 < c2, caxis([c1 c2]) ; end
  %set(S,'LineStyle','-','LineWidth',0.01); %set(S,'EdgeColor','none');
  %set(S,'Clipping','off');
  %get(S) ;
  %nbsf = nbsf+1 ; if nbsf == 1 ; hold on ; view(0,90) ; end
  nbsf = nbsf+1 ; if nbsf == 1 ; hold on ; view(0,90) ; end
   %axis([-180 180 60 90]) ; % work only without coast-line
   %axis([-180 180 -90 -60]) ; % work only without coast-line
   %axis([30 60 -45 -25]) ; % work only without coast-line
return
%----------------------------------------------------------------------

