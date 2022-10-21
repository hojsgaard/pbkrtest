#!/usr/bin/env python3

import sympy
from sympy import *
from sympy.printing.pretty.pretty import PrettyPrinter
from sympy.printing.pretty.pretty_symbology import pretty_use_unicode
from sympy.printing.pretty.stringpict import prettyForm

class MyPrettyPrinter(PrettyPrinter):
    def _print_float(self, e):
       return prettyForm('{:.3f}'.format(e))

def print_custom(expr, **settings):   
    settings['use_unicode'] = False
    
    pp = MyPrettyPrinter(settings)
    #    breakpoint()
    # XXX: this is an ugly hack, but at least it works
    use_unicode = pp._settings['use_unicode']
    uflag = pretty_use_unicode(use_unicode)

    try:
        return print(pp.doprint(expr))
    finally:
        pretty_use_unicode(uflag)



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




