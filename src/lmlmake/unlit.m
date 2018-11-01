module	-- unlit - convert literate source files to ordinary source files.
export unlit;

-- This version does not handle \begin{code} & \end{code}, and it is
-- careless with indentation.

rec unlit = map unlitline

and unlitline ('>'.s) = s
 || unlitline _ = ""

end
