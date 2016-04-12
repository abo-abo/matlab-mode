function emacs_dbstack
st = dbstack(1);
for i = 1:size(st, 1)
    fprintf('<a href="%s:%d">%s:%d</a>\n', which(st(i).file), st(i).line, st(i).file, st(i).line);
end
end