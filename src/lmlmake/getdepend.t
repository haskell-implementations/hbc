import type FileId = NonExistent String + Inode Int Int;
import extractDependencies: ((List (List Char))->((List (FileId # (List (List Char)))) # (List ((List Char) # (List (List Char)))))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
