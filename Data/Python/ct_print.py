import wolframclient.language

from wolframclient.language import wl
from wolframclient.language.side_effects import wl_side_effect as wl_eval

def wl_print(*args):
  wl_eval(wl.PythonSessionPrint(*args))

def str_print(*args):
  wl_eval(wl.PythonSessionPrint(''.join([repr(a) for a in args])))
