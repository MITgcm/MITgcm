% DiagPlotResetAxes is called by DiagPlot and cannot be used seperately.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      (Re)Set color axis and axes                        % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set the coloraxis.  It can either be set to a predefined fixed range (use
% CaxisFixed), the plots minimum and maximum values (use CaxisMinMax), or
% values calculated by MATLAB (set both to zero).  Also check range.
datarange = [min(plotdata(:)),max(plotdata(:))];
try
    eval(['fixedrange = ',fln,'range',flu,';']);
    if datarange(1) < fixedrange(1) | ...
       datarange(2) > fixedrange(2)
        disp(['***Warning***  Value out of range for ',fln]);
        disp(['               Data range:  ',mat2str(datarange)]);
        disp(['               Fixed range:  ',mat2str(fixedrange)]);
    end
catch
    disp(['***Warning***  No range information found.']);
    disp(['               Data range:  ',mat2str(datarange)]);
end
if ~isequal(pst,'Lin')
    try caxis(crange);
    catch
		if CaxisFixed
            if ismember(cmp,{'Sep','Sbs'})
                try
                    eval(['caxis(',fln,'range',flu,');']);
                end
            end
		elseif CaxisMinMax
            if ismember(cmp,{'Sep','Dif','Sbs'})
                caxis([min(plotdata(:)),max(plotdata(:))]);
            end
		end
    end
end

% Reset the axis.  It can either be set to the axis limits (use AxesFixed),
% the x-axis can be trimmed (use AxesTrimX) to avoid the sometimes
% appearing white space, or the axis values calculated by MATLAB can be
% used (use nothing).  CURRENTLY NOT DEFINED FOR CS DATA!!!
if AxesTrimX
    if ~isCS
        set(gca,'xlim',[min(xax{inrow}{incol}(:)),...
                        max(xax{inrow}{incol}(:))]);
    end
end
if AxesTrimY
    if ~ismember(pltslc{inrow}{incol},{'lonfld','latfld','timfld'}) && ~isCS
        set(gca,'ylim',[min(yax{inrow}{incol}(:)),...
                        max(yax{inrow}{incol}(:))]);
    end
end

% Limit y range -- Good for looking at upper layer of ocean.
if isequal(flu,'O') && ~isempty(ylimO) && ...
   isequal(pltslc{inrow}{incol}(4:6),'hgt'), set(gca,'ylim',ylimO); end
if isequal(flu,'A') && ~isempty(ylimA) && ...
   isequal(pltslc{inrow}{incol}(4:6),'hgt'), set(gca,'ylim',ylimA); end
if ~isempty(xlim), set(gca,'xlim',xlim); end
if ~isempty(ylim) && isequal(pltslc{inrow}{incol}(4:6),'fld')
    set(gca,'ylim',ylim); end


% If this is a Dimension-height plot, the axis must be flipped.
if ismember(pltslc{inrow}{incol},{'lonhgt','lathgt'}) && isequal(flu,'A')
    set(gca,'ydir','reverse');
end