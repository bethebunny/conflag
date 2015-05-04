from __future__ import print_function

import conflag
import sys

if __name__ == '__main__':
    cfg = conflag.parse(sys.stdin.read())
    args = sys.argv[1:]
    if args:
        for arg in args:
            ctx = cfg
            for ref in arg.split('.'):
                ctx = ctx[ref]
            print(ctx.native())
    else:
        print(cfg)
