#!/usr/bin/env python3

import sympy
from sympy import *
from sympy.printing.pretty.pretty import PrettyPrinter
from sympy.printing.pretty.pretty_symbology import pretty_use_unicode
from sympy.printing.pretty.stringpict import prettyForm

class MyPrettyPrinter(PrettyPrinter):

    def __init__(self):
        super().__init__({
            "order": None,
            "full_prec": "auto",
            "use_unicode": False,    # <------- NEW DEFAULT
            "wrap_line": True,
            "num_columns": None,
            "use_unicode_sqrt_char": True,
            "root_notation": True,
            "mat_symbol_style": "plain",
            "imaginary_unit": "i"
        })
    
    def _print_ExpBase(self, e):
        return self._helper_print_function(e.func, e.args)

    def _print_Exp1(self, e):      
        return prettyForm("exp(1)")

def print_custom(expr):
    
    sympy.init_printing(use_unicode = False)
    print(MyPrettyPrinter().doprint(expr))

n = symbols("n")
f = (1 + 1/n)**n
lim = Limit(f, n, oo)

print("pprint:")
pprint(lim, use_unicode = True)
print("\n")

print("pprint no unicode:")
pprint(lim, use_unicode = False)
print("\n")

print("print_custom:")
print_custom(lim)
print("\n")

print("pprint:")
pprint(lim, use_unicode = True)
print("\n")

##################################################################################




