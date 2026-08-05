function [fac]=grph_CS(var,xcs,ycs,xcg,ycg,c1,c2,shift,cbV,AxBx,kEnv)
% [fac]=grph_CS(var,xcs,ycs,xcg,ycg,c1,c2,shift[,cbV,AxBx,kEnv]) :
% produce a flat plot of the cube_sphere field "var",
%  keeping the initial grid (no interpolation, use "surf")
% xcs,ycs,xcg,ycg = center + corner grid point coordinates
% c1 < c2 = min & max for the color graph
% c1 > c2 = scale with min,max of the field, + c1/100 and + c2/100
% shift=-1 : No coast-line
%        1 : No shift but draw coast-line calling draw_coast
%     else : draw coast-line (using m_proj) shifted by "shift" degree.
% cbV = 0,1 : horizontal,vertical colorbar; >= 2 : no colorbar;
% kEnv = 0 : standard ; =odd : do not draw the mesh ; >1 : no min,Max written.
% AxBx = do axis(AxBx) to zoom in Box "AxBx" ; only if shift=-1,1 ;
%-----------------------

% Written by jmc@mit.edu, 2005.

%- for debugging, switch to > 0:
dBug=0;
%- small number (relative to lon,lat in degree)
epsil=1.e-6;
%- big jump in longitude from 2 neighbor points
xJump=240; xJmp2=xJump*0.5;
%- mid-longitude of the grid (truncated @ epsil level):
xMid=mean(xcs(:)); xMid=epsil*round(xMid/epsil);
if dBug > 0, fprintf(' mid longitude of the grid: xMid= %22.16e\n',xMid); end

if nargin < 9, cbV=0 ; end
if nargin < 10, AxBx=[xMid-180 xMid+180 -90 90] ; end
if nargin < 11, kEnv=0 ; end

%------------------------------
 n1h=size(var,1); n2h=size(var,2);
 if n1h == 6*n2h, nc=n2h;
 elseif n1h*6 == n2h, nc=n1h;
 else
  error([' input var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
 end
  ncp=nc+1 ; nPg=nc*nc*6;
  MxV=min(min(var));
  mnV=max(max(var));
 if shift == 1 | shift == -1,
  xxt=reshape(xcs,[nPg 1]); yyt=reshape(ycs,[nPg 1]);
  tmp=(xxt-AxBx(1)).*(AxBx(2)-xxt); [I]=find(tmp > 0 );
  tmp=(yyt(I)-AxBx(3)).*(AxBx(4)-yyt(I)); [J]=find(tmp > 0 );
  IJ=I(J); tmp=var(IJ); mnV=min(tmp); MxV=max(tmp);
 end
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
  fac=pi/180.;
else
  fac=1. ;
end
%---
nbsf = 0 ; ic = 0 ; jc = 0 ;
nPx=prod(size(xcg)); nPy=prod(size(ycg));
if nPx == nPg & nPy == nPg,
 %- when stored in long-vector, use "compact" convention (i.e., 1 face after the other)
 if n2h == nc,
   xcg=permute(reshape(xcg,[nc 6 nc]),[1 3 2]);
   ycg=permute(reshape(ycg,[nc 6 nc]),[1 3 2]);
 end
 xcg=reshape(xcg,[nPg 1]); ycg=reshape(ycg,[nPg 1]);
%- add the 2 missing corners:
%fprintf(' Local version of grph_CS : ---------------------------------- \n');
 xcg(nPg+1)=xcg(1); ycg(nPg+1)=ycg(1+2*nc*nc);
 xcg(nPg+2)=xcg(1+3*nc*nc); ycg(nPg+2)=ycg(1);
elseif nPx ~= nPg+2 | nPy ~= nPg+2,
 error([' wrong xcg,ycg dimensions : ',int2str(nPx),' , ',int2str(nPy)]);
end
[x6c]=split_C_cub(xcs,0);
[xx2]=split_Z_cub(xcg);
[yy2]=split_Z_cub(ycg);
%---
if n2h == nc,
 var=permute(reshape(var,[nc 6 nc]),[1 3 2]);
else
 var=reshape(var,[nc nc 6]);
end
for n=1:6,
 %if n < 5 & n > 2,
 if n < 7,
%--------------------------------------------------------
  vv1=zeros(ncp,ncp) ; xx1=vv1 ; yy1=vv1 ;
  vv1(1:nc,1:nc)=var(:,:,n);
  xx1=xx2(:,:,n); yy1=yy2(:,:,n); xxc=x6c(:,:,n);
% if xx1(ncp,1) < xMid-300. ; xx1(ncp,1)=xx1(ncp,1)+360. ; end
% if xx1(1,ncp) < xMid-300. ; xx1(1,ncp)=xx1(1,ncp)+360. ; end
%------------
 %- use jump in grid-cell center longitude to decide where to cut 1 face:
 cutFace=0;
 dxi=xxc(2:nc,:)-xxc(1:nc-1,:); dxImx=max(abs(dxi(:)));
 dxj=xxc(:,2:nc)-xxc(:,1:nc-1); dxJmx=max(abs(dxj(:)));
 if dxImx > xJump & dxJmx > xJump, cutFace=3;
 elseif dxImx > xJump, cutFace=1;
 elseif dxJmx > xJump, cutFace=2; end
 if dBug > 0,
   fprintf(' face # %i , Max dxI,dxJ = %8.3f , %8.3f',n,dxImx,dxJmx);
   if cutFace > 0, fprintf(' ; cutFace= %i',cutFace); end
   fprintf('\n');
 end
 if cutFace == 3,
  fprintf(' Jump in both i & j not implemented ==> skip face # %i\n',n);
%------------
 elseif cutFace == 2,
  [I,J]=find( abs(dxj) > xJump );
  if dBug > 1, for l=1:length(I), fprintf(' i,j= %2i, %2i \n',I(l),J(l)); end; end
  if min(J) == max(J),
   jc =J(1); jp=jc+1;
   if dBug > 0, fprintf('--> cut Face @ jc,jp = %3i,%3i\n',jc,jp); end
%   cut the face in 2 parts (1: j in [1 jp] ; 2: j in [jp ncp]) and plot separately
   for lp=1:2,
    if lp == 1,
      j1=1; j2=jp;
    else
      j1=jp; j2=ncp;
    end
    tmp=xxc(:,j1:j2-1); xLoc=mean(tmp(:));
    if dBug > 0, fprintf('    p.%i, %2i -> %2i : %8.3f %8.3f %8.3f\n', ...
                          lp,j1,j2-1,min(tmp(:)),xLoc,max(tmp(:))); end
    if xLoc > xMid,
%-- case where Xc jump from > 180 to < -180 when j goes from jc to jc+1 :
     [I]=find(xx1(:,jp) < xMid-xJmp2); xx1(I,jp)=xx1(I,jp)+360.;
%   Pole longitude is arbitrary: set to +90 to get a nicer plot:
     xx1(find( abs(yy1) > 90-epsil ))=xMid+90;
    else
%-- case where Xc jump from < -180 to > 180 when j goes from jc to jc+1 :
     [I]=find(xx1(:,jp) > xMid+xJmp2); xx1(I,jp)=xx1(I,jp)-360.;
%   Pole longitude is arbitrary: set to -90 to get a nicer plot:
     xx1(find( abs(yy1) > 90-epsil ))=xMid-90;
    end
    [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,j1,j2,c1,c2) ;
   end
  else
   fprintf(' Irregular cut not implemented ==> skip face # %i\n',n);
  end
%------------
 elseif cutFace == 1,
  [I,J]=find( abs(dxi) > xJump );
  if dBug > 1, for l=1:length(I), fprintf(' i,j= %2i, %2i \n',I(l),J(l)); end; end
  if min(I) == max(I),
   ic =I(1); ip=ic+1;
   if dBug > 0, fprintf('--> cut Face @ ic,ip = %3i,%3i\n',ic,ip); end
%   cut the face in 2 parts (1: i in [1 ip] ; 2: i in [ip ncp]) and plot separately
   for lp=1:2,
    if lp == 1,
      i1=1; i2=ip;
    else
      i1=ip; i2=ncp;
    end
    tmp=xxc(i1:i2-1,:); xLoc=mean(tmp(:));
    if dBug > 0, fprintf('    p.%i, %2i -> %2i : %8.3f %8.3f %8.3f\n', ...
                          lp,i1,i2-1,min(tmp(:)),xLoc,max(tmp(:))); end
    if xLoc > xMid,
%-- case where X jump from > 180 to < -180 when i goes from ic to ic+1 :
     [J]=find(xx1(ip,:) < xMid-xJmp2); xx1(ip,J)=xx1(ip,J)+360.;
%   Pole longitude is arbitrary: set to +90 to get a nicer plot:
     xx1(find( abs(yy1) > 90-epsil ))=xMid+90;
    else
%-- case where X jump from < -180 to > 180 when i goes from ic to ic+1 :
     [J]=find(xx1(ip,:) > xMid+xJmp2); xx1(ip,J)=xx1(ip,J)-360.;
%   Pole longitude is arbitrary: set to -90 to get a nicer plot:
     xx1(find( abs(yy1) > 90-epsil ))=xMid-90;
    end
    [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,i1,i2,1,ncp,c1,c2) ;
%-
   end
  else
   fprintf(' Irregular cut not implemented ==> skip face # %i\n',n);
  end
%------------
 else
%-- plot the face in 1 piece :
  xLoc=mean(xxc(:));
  xx1=rem(xx1-xLoc+180*3,360)+xLoc-180;
  [nbsf,S(nbsf)]=part_surf(nbsf,fac,xx1,yy1,vv1,1,ncp,1,ncp,c1,c2) ;
 end
%--------------------------------------------------------
end ; end
 set(S,'LineStyle','-','LineWidth',0.01);
 if rem(kEnv,2) > 0, set(S,'EdgeColor','none'); end
hold off
if shift == -1,
  axis(AxBx); fprintf('  Axis(Box): %i %i %i %i \n',AxBx);
elseif shift == 1,
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
 if cbV < 2, bV=fix(cbV); moveHV_colbar([10+bV*2.2 10 7-5*bV 7+2*bV]/10,cbV); end
if mnV < MxV & kEnv < 2,
 ytxt=min(1,fix(cbV));
 if shift == 1 | shift == -1,
  pos=get(gca,'position');
  xtxt=mean(AxBx(1:2)) ; ytxt=AxBx(3)-(AxBx(4)-AxBx(3))*(16-pos(4)*7.4)/100;
 %fprintf(' pp= %9.6f , ytxt= %7.2f\n',pos(4),ytxt);
 else
  xtxt=60 ; ytxt=30*ytxt-145 ;
 %xtxt= 0 ; ytxt=30*ytxt-120 ;
 end
 %fprintf('min,Max= %9.5g  , %9.5g (xtxt,ytxt= %f %f)\n',mnV,MxV,xtxt,ytxt);
  text(xtxt*fac,ytxt*fac,sprintf('min,Max= %9.5g  , %9.5g', mnV, MxV))
  %set(gca,'position',[-.1 0.2 0.8 0.75]); % xmin,ymin,xmax,ymax in [0,1]
elseif mnV < MxV, fprintf('min,Max= %9.5g  , %9.5g \n',mnV,MxV);
else fprintf('Uniform field: min,Max= %g \n',MxV); end
return
%----------------------------------------------------------------------
function [nbsf,S]=part_surf(nbsf,fac,xx,yy,vv,i1,i2,j1,j2,c1,c2)
  S=surf(fac*xx(i1:i2,j1:j2),fac*yy(i1:i2,j1:j2), ...
         zeros(i2+1-i1,j2+1-j1),vv(i1:i2,j1:j2)) ;
  %shading flat ; %- or faceted (=default) or interp
  if c1 < c2, caxis([c1 c2]) ; end
  %set(S,'LineStyle','-','LineWidth',0.01); set(S,'EdgeColor','none');
  %set(S,'Clipping','off');
  %get(S) ;
  %nbsf = nbsf+1 ; if nbsf == 1 ; hold on ; view(0,90) ; end
  nbsf = nbsf+1 ; if nbsf == 1 ; hold on ; view(0,90) ; end
   %axis([-180 180 60 90]) ; % work only without coast-line
   %axis([-180 180 -90 -60]) ; % work only without coast-line
   %axis([30 60 -45 -25]) ; % work only without coast-line
return
%----------------------------------------------------------------------

