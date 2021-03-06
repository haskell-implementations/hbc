import _decodef: (_Double->(_Integer # _Int)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _encodef: (_Integer->(_Int->_Double)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import _stof: ((_List _Char)->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _itof: (_Int->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _fabs: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _floor: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _ceil: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _tanh: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _cosh: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _sinh: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _gamma: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _atan2: (_Double->(_Double->_Double)) {# ARITY _ = 2 #}{# STRICTNESS _ = "0&1,F" ST #};
import _atan: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _acos: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _asin: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _sqrt: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _log: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,F" ST #};
import _tan: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _cos: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _sin: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _exp: (_Double->_Double) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _ftosf: (_Int->(_Int->(_Double->(_List _Char)))) {# ARITY _ = 3 #}{# STRICTNESS _ = "0&1&2,0&1&2" ST #};
import _ftos: (_Double->(_List _Char)) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
import _ftoi: (_Double->_Int) {# ARITY _ = 1 #}{# STRICTNESS _ = "0,0" ST #};
