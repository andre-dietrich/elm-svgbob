"""Regenerate the wideEastern table in src/WChar.elm.

Uses the Unicode data bundled with Python:
    python3 scripts/wchar_tables.py && elm-format src/WChar.elm --yes
"""

import re
import sys
import unicodedata

PATH = "src/WChar.elm"


def ranges(test):
    out, start = [], None
    for cp in range(sys.maxunicode + 1):
        if test(chr(cp)):
            start = cp if start is None else start
        elif start is not None:
            out.append((start, cp - 1))
            start = None
    return out


def name(cp):
    return unicodedata.name(chr(cp), "(nil)").title()[:24].ljust(24)


def table(rows):
    lines = []
    for i, (a, b) in enumerate(rows):
        sep = "[" if i == 0 else ","
        lines.append(f"        {sep} ( 0x{a:04X}, 0x{b:04X} ) -- {name(a)}..{name(b)}".rstrip())
    return "\n".join(lines) + "\n        ]"


wide = ranges(lambda c: unicodedata.east_asian_width(c) in ("W", "F"))

src = open(PATH).read()
src = re.sub(r"-- Unicode version '[^']*'", f"-- Unicode version '{unicodedata.unidata_version}'", src)
for fn, rows in (("wideEastern", wide),):
    src = re.sub(
        rf"({fn} =\n    Array\.fromList\n).*?\n        \]",
        lambda m: m.group(1) + table(rows),
        src,
        flags=re.S,
    )
open(PATH, "w").write(src)
