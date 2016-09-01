function em_whos()
    s=evalin('caller','who');
    for i=1:numel(s)
        disp(s{i});
    end
end