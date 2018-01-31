function scalHV_colbar(barscale,ori_h0v1)
% scalHV_colbar(barscale,[ori_h0v1])
% Draw Horizontal(default) or Vertical (ori_h0v1=1) Colorbar 
%  with scale factor given by barscale
% e.g.: barscale=[0.8 0.7] --> shift to center AND reduce by 0.2 in H & 0.3 in V
% e.g.: barscale=[1. .7 .6 .5] --> shift to center by 0.3 in V
%                            AND reduce by 0.4 in H & 0.5 in V

% Written by jmc@ocean.mit.edu, 2006.
if (nargin < 2), ori_h0v1 = 0; end
%---
barfac=barscale;
if size(barscale,2) < 2, barfac(2)=barfac(1) ; end
if size(barscale,2) < 3, barfac(3)=barfac(1) ; end
if size(barscale,2) < 4, barfac(4)=barfac(2) ; end
%---
if ori_h0v1 == 1
  BB=colorbar('vertical') ;
else
  BB=colorbar('horiz') ;
end
pos=get(BB,'position') ;
%fprintf(' -- initial colorbar position : %6.4f %6.4f %6.4f %6.4f \n', pos);
pos(1) = (0.5-pos(1)-pos(3)*0.5)*barfac(1) ;
pos(2) = (0.5-pos(2)-pos(4)*0.5)*barfac(2) ;
pos(3) = pos(3)*barfac(3) ;
pos(4) = pos(4)*barfac(4) ;
pos(1) = 0.5-pos(1)-pos(3)*0.5 ;
pos(2) = 0.5-pos(2)-pos(4)*0.5 ;
%fprintf(' - modified colorbar position : %6.4f %6.4f %6.4f %6.4f \n', pos);
if pos(3) < 0 | pos(4) < 0 , 
  errmsg=sprintf(' colorbar: width or height must be > 0: %6.4f %6.4f', pos(3:4));
  error(errmsg)
end
set(BB,'position',pos);   
return
