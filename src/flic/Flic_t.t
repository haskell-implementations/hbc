import type Name == String;
import type Annot = Annot0 Name + Annot1 Name Flic;
import type Flic = Fname Name + Fnumber Int + Fchar Char + Fstring String + Fap Flic Flic + Flam Name Flic + Flet Bool (List Name) (List Flic) Flic + Fannot Annot Flic + Ffail;
