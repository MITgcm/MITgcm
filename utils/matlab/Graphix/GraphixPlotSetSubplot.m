% DiagPlotSetSubplot is called by DiagPlot and cannot be used seperately.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Initialize subplot in figure                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Determine number of plot rows, columns, xi, yi, dx, dy.
if ismember(cmp,{'Sep','OvE','OvC','Dif'})
    nrows = nsp;
    ncols = 1;
    dx = 1-dxl-dxr;
    dy = (1-dyb-dyt-(nrows-1)*dym)/nrows;
    xi = dxl;
    yi = 1-dyt-isp*dy-(isp-1)*dym;
elseif isequal(cmp,'Sbs')
    nrows = nsp/2;
    ncols = 2;
    dx = (1-dxl-dxr-dxm)/2;
    dy = (1-dyb-dyt-(nrows-1)*dym)/nrows;
    xi = dxl + mod((isp-1),2)*(dx+dxm);
    yi = 1-dyt-ceil(isp/2)*dy-(ceil(isp/2)-1)*dym;
else
    error(['Undefined comparison type:  ',cmp]);
end
cxi = xi+dx-dxcb;


% Initiate subplot.
subplot(nrows,ncols,isp);
set(gca,'position',[xi,yi,dx,dy]);
set(gca,'fontsize',fs_tick);
hold on;
