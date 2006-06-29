% DiagPlotMakePlot is called by DiagPlot and cannot be used seperately.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Make plot                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if isequal(cmp,'Dif')
    plotdata = data{inrow}{1} - data{inrow}{2};
else
    plotdata = data{inrow}{incol};
end
plotdata = plotdata + offset;
% [min(plotdata(:)),max(plotdata(:))]
% size(plotdata)
% size(xax{inrow}{incol})
% size(yax{inrow}{incol})
% plotdata
% x = xax{inrow}{incol};
% y = yax{inrow}{incol};
% save('Dump.mat','plotdata','x','y');

% Make gridded plot.  This should be fixed up to allow for non-constant
% height intervals, imagesc makes everything equally spaced in the vertical
% (and in the horizontal for that matter).
if isequal(pst,'Grd')
    if isCS
        merccube_mod(xax{inrow}{incol},yax{inrow}{incol},plotdata);
    else
        imagesc(xax{inrow}{incol},yax{inrow}{incol},plotdata);
    end
    if isequal(Shading,'interp')
        shading interp;
    end
    
% Make interpolated plot.
elseif isequal(pst,'Int')
    if isCS
        merccube_mod(xax{inrow}{incol},yax{inrow}{incol},plotdata);
        shading interp;
    else
        pcolor(xax{inrow}{incol},yax{inrow}{incol},plotdata);
        shading interp;
    end
    
% Make contour (non-filled) plot.
elseif isequal(pst,'Con')
    hold off;
    if isequal(cmap,'solid-dashed')
        for ii = 1:3
            cint = contint; lw = linewidth;
            pr = [min(plotdata(:)),max(plotdata(:))];
            if ii == 1, cint = cint(cint>0); lst = '-'; end
            if ii == 2, cint = cint(cint<0); lst = '--'; end
            if ii == 3, cint = [0,0]; lst = '-'; lw = 2.*lw; end
            Lines = 0;
            for icint = 1:length(cint)
                Lines = Lines | (cint(icint)>pr(1) & cint(icint)<pr(2));   
            end
            if Lines
                [cs,h] = contour(xax{inrow}{incol},...
                                 yax{inrow}{incol},...
                                 plotdata,cint,['k',lst]);
                set(h,'linewidth',lw);
                if UseConLabel
                    clabel(cs,h,clabelv,'fontsize',fs_clabel,'rotation',...
                           0,'labelspacing',labelspacing);
                end
                hold on;
            end
        end
    else
        [cs,h] = contour(xax{inrow}{incol},...
                         yax{inrow}{incol},...
                         plotdata,contint);
        return
        set(h,'linewidth',linewidth);
        if UseConLabel
            clabel(cs,h,clabelv,'fontsize',fs_clabel,'rotation',0,...
                   'labelspacing',labelspacing);
        end
    end
    
% Make contour (filled) plot.  Let MATLAB determine the contour
% intervals on a differencing plot.
elseif isequal(pst,'Cnf')
    try   [cs,h] = contourf(xax{inrow}{incol},yax{inrow}{incol},plotdata,contint);
    catch [cs,h] = contourf(xax{inrow}{incol},yax{inrow}{incol},plotdata); end
    if UseCnfLabel
        clabel(cs,h,clabelv,'fontsize',fs_clabel,'rotation',0,...
               'labelspacing',labelspacing);
    end
    
% Make line plot.  If this is an overlaying comparison, cycle through the
% appropriate data of which to overlay.
elseif isequal(pst,'Lin')
    if ismember(cmp,{'OvC','OvE','OvF'})
        for intrl = 1:ntrl
            plot(xax{inrow}{intrl},data{inrow}{intrl},...
                 linecolors{intrl},'linewidth',linewidth);
        end
    else
        plot(xax{inrow}{incol},plotdata,linecolors{1},'linewidth',linewidth);
    end
    
% Unknown plot style encountered.
else
    error(['Oh dear!  Undefined PlotStyle:  ',pst]);
end
