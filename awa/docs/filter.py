#!/usr/bin/python3
from pandocfilters import toJSONFilter, Math, Para, Link
import sys
"""
Pandoc filter to convert gitlab flavored markdown to pandoc flavored markdown
"""


def gitlab_markdown(key, value, format, meta):
    if key == "CodeBlock":
        [[identification, classes, keyvals], code] = value
        if len(classes) > 0 and classes[0] == "math":
            fmt = {'t': 'DisplayMath',
                   'c': []}
            return Para([Math(fmt, code)])

    elif key == "Link":
        if len(value) <= 2:
            return None
        # sys.stderr.write("Raw ")
        # sys.stderr.write(str(len(value)))
        # sys.stderr.write(str(value))
        # sys.stderr.write("\n")
        link = value[2]
        if str(link[0]).endswith(".md"):
            link = '#'
            needSep = 0
            for item in value[1]:
                if 't' in item:
                    if item['t'] == 'Str':
                        if needSep != 0:
                            link += '-'
                        link += item['c'].lower()
                        needSep = 1
            # sys.stderr.write(str(link))
            value[2] = [link, '']

if __name__ == "__main__":
    toJSONFilter(gitlab_markdown)
