module Termcap(noColumns, noLines, clear, moveTo) where
import LMLtermcap
noColumns = _Columns
noLines = _Lines
clear = _Clear
moveTo x y = _MoveTo x y
