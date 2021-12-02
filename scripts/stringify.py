#!/usr/bin/env python3
import sys

sys.stdout.buffer.write(b'"')

data = sys.stdin.buffer.read()
data = data.replace(b'\\', b'\\\\')
data = data.replace(b'"', b'\\"')
sys.stdout.buffer.write(data)
sys.stdout.buffer.write(b'"')
