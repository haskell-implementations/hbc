import isiso: (Char->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import dropqual: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import get_id_kind: (Id->Kind) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import preludeBuiltin: String {# ARITY _ = 0 #}{# STRICTNESS _ = "T,F" ST #};
import getspec: (Id->(Option (List (#3 Id Ttype Finfo)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import specialpre: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import idtopstr: (Id->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import optparen: ((List Char)->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import tupstr: (Int->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "T,T" ST #};
import getminame: (Modinfo->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import dprid: (Id->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import entries_of_id: (Id->(List (List (Arginfo # Argpos)))) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import inprelude: ((Modinfo # *a)->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import updidname: (Id->(String->Id)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import id_visibility: (Id->Visibility) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import updvis: (Visibility->(Id->Id)) {# ARITY _ = 2 #}{# STRICTNESS _ = "1,F" ST #};
import id_metsel: (Id->(List Int)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_metarity: (Id->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import preludename: String {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import id_isconstr: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_issyn: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_isinst: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_isclass: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import mknewids: ((List Char)->(Int->((List Char)->Id))) {# ARITY _ = 3 #}{# STRICTNESS _ = "T,T" ST #};
import mknewid: ((List Char)->(Int->Id)) {# ARITY _ = 2 #}{# STRICTNESS _ = "T,T" ST #};
import id_ismethod: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_isvar: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_fixity: (Id->Fixity) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_orignames: (Id->(Modinfo # String)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import isdummy: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_no: (Id->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_is_predef: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import type_of_id: (Id->Otype) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import id_is_visible: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import noorigname: Origname {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import id_is_global: (Id->Bool) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import nprid: (Id->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import mkpids: ((List Char)->Id) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import framesize_of_id: (Id->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import arity_of_id: (Id->Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import idi_varu: Idinfo {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import asmid: (Id->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import dummyid: Id {# ARITY _ = 0 #}{# STRICTNESS _ = "T,T" ST #};
import idtostr: (Id->String) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import ltid: (Id->(Id->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import eqid: (Id->(Id->Bool)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0,F" ST #};
import oprid: (Id->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import prid: (Id->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import pprid: (Id->(List Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
