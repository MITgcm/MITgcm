function [savI,savJ,savF,isav,jsav,xsav,nMx6t]=find_bk_line( ...
         nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx1,xx2,yy2,xcs,ycs);
% 1rst step : fort each "yl" and for each face;
% find the broken-line closest to yl, starting from the West side = min(x)
%--------------------------------------

if yl == 1, fprintf( ...
 '--- find_bk_line: nf1,nf2,nc,ydim,yl= %i %i %i %i %8.3f\n', ...
                    nf1,nf2,nc,ydim,yl); end
ncp=nc+1;
nMx6t=zeros(6,1);
savI=zeros(nc*6,6); savJ=zeros(nc*6,6); savF=zeros(nc*6,6);
isav=zeros(nc*6,6); jsav=zeros(nc*6,6); xsav=zeros(nc*6,6);

for n=nf1:nf2, 
 
%---
% to find the 1rst point : 
% start from the midle point ; If no point => give up.
% decrease X until encounter boundary : 
% set 1rst point = 1rst point over boundary

 nPts=0; yl1=yl-dylat/2 ; yl2=yl+dylat/2 ;
 [Il,Jl]=find( yy2(:,:,n) >= yl1 & yy2(:,:,n) < yl2) ; Ntloc=size(Il,1); 
if Ntloc > 0,

 xloc=zeros(Ntloc,1); 
 for i=1:Ntloc, xloc(i)=xx2(Il(i),Jl(i),n) ; end
 Xmidle=mean(xloc); xloc=abs(xloc-Xmidle); Xmn=min(xloc);
 ix=find(xloc <= Xmn+dylat); Nxloc=size(ix,1) ;
 yloc=zeros(Nxloc,1);
 for j=1:Nxloc, yloc(j)=yy2(Il(ix(j)),Jl(ix(j)),n) ; end
 yloc=abs(yloc-yl); Ymn=min(yloc) ; jx=find(yloc==Ymn);
 i0=Il(ix(jx(1))); j0=Jl(ix(jx(1)));
 x0=xx2(i0,j0,n) ; y0=yy2(i0,j0,n) ;

 if ydim == 1, fprintf('n=%i, i,j,X,Y(-1)= %2i %2i %8.3f %8.3f ;', ...
                        n,i0,j0,x0,y0); end
%-------------------------------------------------
 while Ymn < XYout/2,

%- save the 1rst point :
  if nPts == 0,
   nPts = 1 ;
  else
%- save the next point :
   i1 = i0 ; j1 = j0 ;
   if i4c(1) == 1, i1 = i0 -1 ; end
   if i4c(1) == 2, i1 = i0 +1 ; end
   if i4c(1) == 3, j1 = j0 -1 ; end
   if i4c(1) == 4, j1 = j0 +1 ; end
   nPts=nPts+1; x1=xx2(i1,j1,n);
   if x1 > x0, x1=x1-360 ; Ymn=XYout; end
   i0 = i1 ; j0 = j1 ; x0=x1; 
  end

  if Ymn < XYout/2,
%- look for the next point : try the 4 direction i-1,i+1,j-1,j+1 :
   y4c=XYout*ones(4,1); y0=yy2(i0,j0,n) ;

   if i0 > 1, xx = xx2(i0-1,j0,n); 
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx < 0, y4c(1)=yy2(i0-1,j0,n) ; end ; end
   if i0 < ncp, xx = xx2(i0+1,j0,n);
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx < 0, y4c(2)=yy2(i0+1,j0,n) ; end ; end
   if j0 > 1, xx = xx2(i0,j0-1,n);
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx < 0, y4c(3)=yy2(i0,j0-1,n) ; end ; end
   if j0 < ncp, xx = xx2(i0,j0+1,n);
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx < 0, y4c(4)=yy2(i0,j0+1,n) ; end ; end
%-- avoid the North & South poles (since Long is not well define there):
    y4c(find( abs(y4c) == 90)) = XYout; 
   y4c=abs(y4c - yl); Ymn=min(y4c) ; i4c=find(y4c==Ymn); 
  end
  %if nPts > 0, 
  %fprintf('nPts,y4c : %i %8.3f %8.3f %8.3f %8.3f ;  x0,y0= %8.3f %8.3f\n', ...
  %    nPts,y4c,x0,y0); end
  %if nPts > 53, return ; end  

 end; % (while)

% x0=xx2(i0,j0,n) ; 
%- x0 is already set to xx2(i0) or to xx2(i0)-360
 y0=yy2(i0,j0,n) ; Ymn=abs(y0-yl);
 if ydim == 1,
   fprintf(' nPts=%3i, i0,j0,x0,y0= %2i %2i %8.3f %8.3f \n', ...
             nPts,i0,j0,x0,y0); end

%-------------------------------------------------

 while Ymn < XYout/2,

%- save the 1rstpoint :
  if nMx6t(n) == 0,
   nMx6t(n) = 1 ; isav(1,n)=i0 ; jsav(1,n) = j0 ;
   xsav(1,n) = x0 ; if xMid(n) ~= 0, xsav(1,n)=xx1(i0,j0,n) ; end

  else
%- save the next point :
   i1 = i0 ; j1 = j0 ; ii=i0 ; jj=j0 ; is=nMx6t(n) ;
   if i4c(1) == 1, i1 = i0 -1 ; savF(is,n)=2 ; ii=i1 ; end
   if i4c(1) == 2, i1 = i0 +1 ; savF(is,n)=2 ; end
   if i4c(1) == 3, j1 = j0 -1 ; savF(is,n)=1 ; jj=j1 ; end
   if i4c(1) == 4, j1 = j0 +1 ; savF(is,n)=1 ; end
   x1=xx2(i1,j1,n); if x1 < x0, Ymn=XYout; savF(is,n)=0; end
  if Ymn < XYout/2,
   savI(is,n)=ii ; savJ(is,n)=jj ;
%- cut !
   if ii == ncp | jj == ncp, savF(is,n)=0 ; end
%- suppress point outside yl1,yl2 :
   if ( mod(i0,nc) == 1 | mod(j0,nc) == 1 ) & ...
      abs(yy2(i0,j0,n)-yl) > dylat/2, savF(is,n)=0 ; end 
   if ( mod(i1,nc) == 1 | mod(j1,nc) == 1 ) & ...
      abs(yy2(i1,j1,n)-yl) > dylat/2, savF(is,n)=0 ; end 

%- if jump in xx1 => cut and add 1 point :
   if xMid(n) ~= 0 &  xx1(i1,j1,n) < xx1(i0,j0,n),
    savF(is+1,n)=savF(is,n); savF(is,n)=0; is=1+is; 
    isav(is,n)=i0 ; jsav(is,n)=j0 ; xsav(is,n)=xx1(i0,j0,n)-360;
    savI(is,n)=ii ; savJ(is,n)=jj ;
   end

   xPmid=(xx1(i1,j1,n) + xsav(is,n))/2;
   yPmid=(yy2(i1,j1,n)+yy2(i0,j0,n))/2;
   ic=min(nc,ii); jc=min(nc,jj);
   xCenter=xcs(ic+(n-1)*nc,jc) ; yCenter=ycs(ic+(n-1)*nc,jc) ;
%- compute vector product p1,p2 x Pmid,Center :
   dirUV = (yCenter - yPmid)*(xx1(i1,j1,n) - xsav(is,n)) ...
         - (xCenter - xPmid)*(yy2(i1,j1,n)-yy2(i0,j0,n)) ;
   if dirUV < 0, savF(is,n)=-savF(is,n) ; end
%  if yCenter <= yMid, savF(is,n)=-savF(is,n) ; end
%  if ydim == 1 & dirUV*(yCenter - yPmid) <= 0 , 
   if dirUV*(yCenter - yPmid) < 0 , 
    fprintf(['- Sign - : n,is,i0,i1,j0,j1= %i %i %i %i %i %i ', ...
     ' ;  dirUV= %e \n'], n,is,i0,i1,j0,j1,dirUV);
    fprintf('           %8.3f %8.3f %8.3f %8.3f \n', ...
                         xsav(is,n),xx1(i1,j1,n),xPmid,xCenter);
    fprintf('           %8.3f %8.3f %8.3f %8.3f \n', ...
                       yy2(i0,j0,n),yy2(i1,j1,n),yPmid,yCenter);
   end

   isav(1+is,n)=i1 ; jsav(1+is,n)=j1 ; xsav(1+is,n)=xx1(i1,j1,n); 
   nMx6t(n)=1+is ; x1=xx2(i1,j1,n);
   i0 = i1 ; j0 = j1 ; x0=x1 ;
  end

  end

  if Ymn < XYout/2,
%- look for the next point : try the 4 direction i-1,i+1,j-1,j+1 :
   y4c=XYout*ones(4,1); y0=yy2(i0,j0,n) ;
  %if xMid(n) == 1, x0=xx2(i0,j0,n) ; end

   if i0 > 1, xx = xx2(i0-1,j0,n); 
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx > 0, y4c(1)=yy2(i0-1,j0,n) ; end ; end
   if i0 < ncp, xx = xx2(i0+1,j0,n);
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx > 0, y4c(2)=yy2(i0+1,j0,n) ; end ; end
   if j0 > 1, xx = xx2(i0,j0-1,n);
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx > 0, y4c(3)=yy2(i0,j0-1,n) ; end ; end
   if j0 < ncp, xx = xx2(i0,j0+1,n);
    ddx= xx-x0+3*180 ; ddx=rem(ddx,360)-180 ; 
    if ddx > 0, y4c(4)=yy2(i0,j0+1,n) ; end ; end
%-- avoid the North & South poles (since Long is not well define there):
    y4c(find( abs(y4c) == 90)) = XYout; 
   y4c=abs(y4c - yl); Ymn=min(y4c) ; i4c=find(y4c==Ymn); 
  end

 end; % end(while)

end ; end
return

