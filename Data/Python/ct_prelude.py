# ct_prelude.py is directly injected at session startup, rather than treated as a module

from ct_print import wl_print, str_print

from importlib import reload
import ct_patch_wc

# from dataclasses import dataclass
# @dataclass
# class Foo():
#   x : int
#   y : bool
#   z : any

# foo = Foo(5, False, None)
# bar = Foo(0, True, None)
# bar.z = bar

import runpy

