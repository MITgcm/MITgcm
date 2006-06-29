% DiagPlotMisc is called by DiagPlot and cannot be used seperately.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Apply desired colorbar, contour label, tick labels, box, grid      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Center caxis around 0
if cmapcenter0
    caxis([-max(abs(caxis)),max(abs(caxis))]);
end

% Apply desired colormap.
if ~isequal(cmap,'')
    if isequal(cmap,'solid-dashed')
        % Accounted for in DiagPlotMakePlot.
    elseif isequal(cmap,'black')
        colormap([0,0,0]);
    elseif isequal(cmap,'bwr')
        load('BWR_6.mat');
        colormap(bwr);
    else
        colormap(cmap);
    end
end

% Add colorbar, except for line plots.  When the colorbar is placed, reset
% the colorbar and axes positions on the figure to match desired settings.
% Colorbar can be turned off as desired.
if UseColorbar
    if ~isequal(pst,'Lin')
        h=colorbar;
        cxi = xi+dx-dxcb;
        set(h,'position',[cxi,yi,dxcb,dy]);
        set(h,'fontsize',fs_colorbar);
        set(gca,'position',[xi,yi,dx-dxcb-dxcbg,dy]);
    end
end

% Reform tick labels.  It the PlotStyle (pst) is 'Lin', do nothing with the
% y-axis label as is it now a dependent variable.
if UseNiceTickLabels
    if isequal(pltslc{inrow}{incol}(1:3),'lon')
        xtick = fac.*lontick; xticklabel = lonticklabel;
        set(gca,'xtick',xtick); set(gca,'xticklabel',xticklabel);       
    elseif isequal(pltslc{inrow}{incol}(1:3),'lat')
        xtick = fac.*lattick; xticklabel = latticklabel;
        set(gca,'xtick',xtick); set(gca,'xticklabel',xticklabel);  
    elseif isequal(pltslc{inrow}{incol}(1:3),'tim')
        % xtick = timtick; xticklabel = timticklabel;
        % set(gca,'xtick',xtick); set(gca,'xticklabel',xticklabel);  
    end

    if ~isequal(pltslc{inrow}{incol}(4:6),'fld')
        if isequal(pltslc{inrow}{incol}(4:6),'lat')
            ytick = fac.*lattick;  yticklabel = latticklabel;
        elseif isequal(pltslc{inrow}{incol}(4:6),'hgt')
            eval(['ytick = vertick',flu,';']);
            eval(['yticklabel = verticklabel',flu,';']);
        end
        set(gca,'ytick',ytick);
        set(gca,'yticklabel',yticklabel);
    end
end
set(gca,'tickdir',TickDir);

% Add box and grid as desired.
eval(['box ' ,Box ,';']);
eval(['grid ',Grid,';']);

% Add legend if comparison is set to one of the overlay settings:  'OvE' or
% 'OvC'.  If the 'UseLegend' flag is turned on but the comparison type is
% not one of these overlay settings, nothing is done.
if UseLegend && ismember(cmp,{'OvC','OvE','OvF'})
    legendstr = '';
    for intrl = 1:ntrl
        if     isequal(cmp,'OvE'), tempname = page{inrow}{intrl}{itrl};
        elseif isequal(cmp,'OvC'), tempname = page{inrow}{intrl}{iavg};
        elseif isequal(cmp,'OvF'), tempname = page{inrow}{intrl}{ifln}; end
        tempname = AddSlashesBeforeUnderscores(tempname);
        legendstr = [legendstr,'''',tempname,''','];
    end
    eval(['legend(',legendstr,num2str(LegendPlacement),')']);
end

% Add coast as appropriate.
if Coast
    m_proj('Equidistant Cylindrical','lat',90,'lon',[-180 180]);
    m_coast('color',[0 0 0]);
end

% Add cube lines.
if Edges
    drawedges(XG{inrow}{incol},YG{inrow}{incol});
end

% Add text box
if ~isequal(TextBox,'')
    h = text(TextPos(1),TextPos(2),TextBox,'units','normalized',...
             'backgroundcolor',[1,1,1],'edgecolor',[0,0,0],'fontsize',...
             fs_textbox,'verticalalignment','bottom');
end