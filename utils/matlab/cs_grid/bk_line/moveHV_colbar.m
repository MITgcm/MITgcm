function moveHV_colbar(barscale,ori_h0v1)
% moveHV_colbar(barscale,[ori_h0v1])
% Draw Horizontal(default) or Vertical (ori_h0v1=1) Colorbar
%  with scale factor given by barscale
% e.g.: barscale=[0.8 0.7] --> shift to center AND reduce by 0.2 in H & 0.3 in V
% e.g.: barscale=[1. .7 .6 .5] --> shift to center by 0.3 in V
%                            AND reduce by 0.4 in H & 0.5 in V

% Written by jmc@mit.edu, 2006.

wr2std=0;
if (nargin < 2), ori_h0v1 = 0; end
nSub=rem(fix(ori_h0v1*1000),1000);
if nSub ~= 0,
 if wr2std > 0, fprintf(' Arg-2= %6.4f ,',ori_h0v1); end
 nsV=fix(nSub/100);
 nsH=fix(nSub/10-nsV*10);
 js=rem(nSub,10)-1;
 is=rem(js,nsH); js=nsV-1-fix(js/nsH);
 if wr2std > 0,
  fprintf(' nSub= %i , nsH,nsV,is,js= %i %i %i %i\n',nSub,nsH,nsV,is,js);
 end
end
ori_h0v1=fix(ori_h0v1);
%---
barfac=barscale;
if size(barscale,2) < 2, barfac(2)=barfac(1) ; end
if size(barscale,2) < 3, barfac(3)=barfac(1) ; end
if size(barscale,2) < 4, barfac(4)=barfac(2) ; end
%---
if ori_h0v1 == 1
% BB=colorbar('vertical');
  BB=colorbar('EastOutside');
else
% BB=colorbar('horiz');
  BB=colorbar('SouthOutside'); % new syntax
end
pos=get(BB,'Position') ;
if pos(3) < 0 | pos(4) < 0, wr2std=1; end
if wr2std > 0,
 fprintf(' -- initial colorbar position : %6.4f %6.4f %6.4f %6.4f \n', pos);end
%- sometimes get Width or Height from colorbar & matlab.7:
if pos(4) < 0, pos(4)=.0312; pos(2)=0.04; end
%if pos(3) < 0, pos(2)=.0312; pos(1)=0.04; end
pos(1) = (0.5-pos(1)-pos(3)*0.5)*barfac(1) ;
pos(2) = (0.5-pos(2)-pos(4)*0.5)*barfac(2) ;
pos(3) = pos(3)*barfac(3) ;
pos(4) = pos(4)*barfac(4) ;
pos(1) = 0.5-pos(1)-pos(3)*0.5 ;
pos(2) = 0.5-pos(2)-pos(4)*0.5 ;
if nSub ~= 0,
 pos(1)=0.03+0.90/nsH+(0.90/nsH)*is;
 pos(2)=0.07+0.15/nsV+(0.89/nsV)*js;
end
if wr2std > 0,
 fprintf(' - modified colorbar position : %6.4f %6.4f %6.4f %6.4f \n', pos);end
if pos(3) < 0 | pos(4) < 0 ,
  errmsg=sprintf(' colorbar: width or height must be > 0: %6.4f %6.4f', pos(3:4));
  error(errmsg)
end
set(BB,'Position',pos);
return
