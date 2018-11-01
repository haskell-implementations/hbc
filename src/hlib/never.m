module
export Pnever, Pshowt;
    Pnever = Pfail "This can never happen!"
and Pshowt = Pfail "showType arg evaluated."
end
