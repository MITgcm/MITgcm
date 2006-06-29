% DiagPlotTitles is called by DiagPlot and cannot be used seperately.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Add titles                                 % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Optionally put on a subplot title.  The subplot title will always contain
% the field name and it's corresponding units.  Optional subplot title
% information that is defined in DiagPlotParam:
%
%   Option (0/1)        Description
%   -------------------------------------------------------------
%   SubplotExp          Experiment
%   SubplotSlc          SliceType information
%   SubplotAvg          Averaging information
%   SubplotPst          PlotStyle information
%   SubplotMinMax       Minimum and maximum
%   SubplotModelTime    Model time
%
% Note that for certain types of SliceType, the longitude ('i=#'), latitude
% ('j=#'), or height ('k=#') information is included as well.  Little
% strings are built up for each title option (left as '' if option is
% turned off), and at the end concatenated together to make the title
% string.  This should make it easy to reorder things as desired.
if SubplotTitle
    info = page{inrow}{incol};
    
    if isequal(SubplotTitleOverride,'')
        % Experiment information.
        trltxt = '';
        if SubplotExp
            trltxt = [info{itrl},'; '];
        end

        % Field and unit information.
        fldtxt = '';
        if SubplotFld
            if isequal(cmp,'OvF')
                fldtxt = ['Fld = Various [',units,']; '];
            else
                fldtxt = ['Fld = ',fln,' [',units,']; '];
            end
        end

        % Model time information.
        timtxt = '';
        if SubplotModelTime
            y1 = num2str(time{inrow}{incol}(1));
            y2 = num2str(time{inrow}{incol}(end));
            timtxt = ['Yrs = [',y1,',',y2,']; '];
        end

        % Configuration information.
        contxt = '(';
        if SubplotSlc
            slc = info{islc};
            contxt = [contxt,'Slc = ''',slc,''', '];
            index = str2num(slc(3:end));
            if isequal(slc(1:2),'i=')
                contxt = [contxt,'lon = ',num2str(XL(index)),', '];
            elseif isequal(slc(1:2),'j=')
                contxt = [contxt,'lat = ',num2str(YL(index)),', '];
            elseif isequal(slc(1:2),'k=')
                contxt = [contxt,'pre = ',num2str(ZC(index)),', '];
            end
        end
        if SubplotAvg
            if isequal(cmp,'OvC')
                contxt = [contxt,'Avg = ''Various'', '];
            else
                contxt = [contxt,'Avg = ''',page{inrow}{incol}{iavg},''', '];
            end
        end
        if SubplotPst
            contxt = [contxt,'Pst = ''',pst,''', '];
        end
        if SubplotSlc | SubplotAvg | SubplotPst
            contxt = [contxt(1:end-2),'); '];
        else
            contxt = '';
        end

        % Data minimum/maximum information.
        minmaxtxt = '';
        if SubplotMinMax
            clear mn mx;
            if ismember(cmp,{'OvC','OvE','OvF'})
                for intrl = 1:ntrl
                    mn(intrl) = min(data{inrow}{intrl}(:));
                    mx(intrl) = max(data{inrow}{intrl}(:));
                end
                mn = num2str(min(mn(:)));
                mx = num2str(max(mx(:)));
            else
                mn = num2str(min(data{inrow}{incol}(:)));
                mx = num2str(max(data{inrow}{incol}(:)));
            end
            minmaxtxt = ['[Min,Max] = [',mn,',',mx,']; '];
        end

        % Throw on the subplot title.
        titlestr = [trltxt,fldtxt,timtxt,contxt,minmaxtxt];
        titlestr = AddSlashesBeforeUnderscores(titlestr(1:end-2));
    else
        titlestr = SubplotTitleOverride;
    end
    title(titlestr,'fontsize',fs_sptitle);
end

% Optionally put on overall figure title containing user defined figure
% title and date of when the analysis was run (not the when the actual
% experiment was run).  This is added as a text string to the first
% subplot.
if FigureTitle
    if isp == 1
        xi = 0;
        yi = 1+titlefac*(dyt/dy);
        titlestr = [pagename,' (Date = ',date,')'];
        titlestr = AddSlashesBeforeUnderscores(titlestr);
        text(xi,yi,titlestr,'units','normalized','fontsize',fs_title);
    end
end

% Add x and y axis labels.
if ~isequal(XLabel,'')
    xlabel(XLabel,'fontsize',fs_axislabel);
end
if ~isequal(YLabel,'')
    ylabel(YLabel,'fontsize',fs_axislabel);
end
