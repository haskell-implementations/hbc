import filter: ((*a->Bool)->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import unindent: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import lines: ((List Char)->(List (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import chop: ((List Char)->(List (List Char))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import lmldir: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import hbc_library: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import getenvdef: ((List Char)->((List Char)->(List Char))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import getenvf: (((List Char)->*a)->((List Char)->(*a->*a))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,F" ST #};
import getpath: ((List Char)->((List (List Char))->(List (List Char)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,F" ST #};
import includePath: (List (List Char)) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import libraryPath: (List (List Char)) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import sourcePath: (List (List Char)) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import inPath: ((List (List Char))->((List Char)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import notFromHbcLibrary: ((List Char)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import notPrelude: ((List Char)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import rmqualified: ((List String)->(List String)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import hbcImport: (Bool->((List Char)->(List (List Char)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import prefixin: ((List *a)->((List *a)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import is_one_of: ((List (List *a))->((List *a)->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import dropTo: ((List *a)->((List *a)->(List *a))) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import specialFlags: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import lmlImport: (Bool->((List Char)->(List (List Char)))) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import distributeFlag: Bool {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import parallelFlag: Bool {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import parseImports: (Bool->((List Char)->(List (List Char)))) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import getflag: ((List Char)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,F" ST #};
import lmlc: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import hbc: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import libraryPathVar: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import compilerFlags: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import compiler: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import importKind: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import ifaceExt: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import litSrcExt: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import srcExt: (List Char) {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
