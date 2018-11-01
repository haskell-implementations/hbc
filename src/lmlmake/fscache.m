module -- fscache
#include "utils.t"
#include "table.t"

rec type Catalog *a == Table (Entry *a)

and type Entry *a == Name # *a

and type Name == String

and type Node *a = Branch (Catalog (Node *a)) + Leaf *a

and show_Node sl n =
    case n
    in Branch es:
       let es' = filter (\(n,_).n~="." & n~="..") (listTable es)
       in "Branch "@show_list (show_pair (show_string,show_Node sl)) es'
    || Leaf l: "Leaf "@sl l
    end

and mapNode f n =
    case n
    in Branch es: Branch (mapTable (asnd (mapNode f)) es)
    || Leaf l: Leaf (f l)
    end

and prune d n =
    case n
    in Branch es: if d<=0
		  then Branch emptyTable
		  else Branch (mapTable (asnd (prune (d-1))) es)
    || Leaf l: Leaf l
    end

and lookup [] n = Yes n
 || lookup (name.path) n =
    case n
    --in Branch es: assocdef name (map (asnd (lookup path)) es) (No (name.path))
    in Branch es: tableLookup (No (name.path)) (lookup path o snd) name es
    || _: No (name.path)
    end

and filesystem root = entry root root

and entry path name = (name,node path)

and node path =
      case statfile path
      in No err: Leaf ([],\x.No err) -- hmm
      || Yes stat:
           if isDir stat
	   then case opendir path
		in No err: fail err
		|| Yes names: Branch (table [entry (path@"/"@name) name;;name<-names])
		end
	   else Leaf (stat,\x.openfile path)
      end


#define _S_IFMT 61440
#define _S_IFDIR 16384
#define S_ISDIR(mode) (bitand (mode) _S_IFMT = _S_IFDIR)

and isDir stat =
    let mode = select 3 stat
    in S_ISDIR(mode)

and dirtree = concmap (\(d,s).space d@s@"\n") o tree 0

and tree d n =
    case n
    in Branch es: concmap (treeEntry d) (listTable es)
    || Leaf l: []
    end

and treeEntry d (s,n) =
    if s="." | s=".." then [] else (d,s).tree (d+2) n

and dir = dirtree o node

and fsroot = node "/"
and fscur  = node "."

and lookupfile s =
    case choplist (splitat '/') (rmdupslash s)
    in "".path: lookup path fsroot
    || path: lookup path fscur
    end

and rmdupslash "" = ""
 || rmdupslash ('/'.'/'.s) = rmdupslash ('/'.s)
 || rmdupslash (c.s) = c.rmdupslash s

and fsopenfile s =
    case lookupfile s
    in Yes (Leaf (_,f)): f []
    || _ : No ("fsopenfile: not a plain file")
    end

and fsstatfile s =
    case lookupfile s
    in Yes (Leaf ([],_)): No "fsstatfile: file not found"
    || Yes (Leaf (s,_)): Yes s
    || _ : No "fsstatfile: not a plain file"
    end

end
