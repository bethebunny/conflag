#!/usr/bin/env python

import argparse
import conflag
import os
import shutil
import subprocess


class BuildContext(object):
    def __init__(self, build_dir):
        self.build_dir = build_dir
        self.built = set()

    def build(self, target):
        if target in self.built:
            return
        self.built.add(target)
        for dependency in target.dependencies:
            self.build(dependency)
        print 'Executing $({})'.format(' '.join(target.build))
        try:
            subprocess.check_call(target.build)
        except OSError as e:
            raise Exception('Failed while executing $({}): {!r}'.format(
                ' '.join(target.build), e))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='conflag based make files')
    parser.add_argument('-m', '--makefile', default='makefile.cfg')
    parser.add_argument('-o', '--build_dir', default='build')
    parser.add_argument('targets', nargs='*', default=['all'])
    args = parser.parse_args()

    with open(args.makefile) as makefile:
        parsed = conflag.parse(makefile.read())

    with open(args.makefile) as makefile:
        cfg = conflag.loads(makefile.read())

    ctx = BuildContext(args.build_dir)
    for target in args.targets:
        ctx.build(cfg.targets[target])
