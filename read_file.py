"""Backend for the `read_file' tool; runs inside the sandbox.

Usage: python3 read_file.py PATH OFFSET LIMIT MAXBYTES
Prints lines OFFSET.. (1-based) of PATH: at most LIMIT lines (0 = no
limit) and at most MAXBYTES bytes of content, cut at a line boundary.
"""
import sys, os

def out(s): sys.stdout.write(s + "\n")
def die(s): out(s); sys.exit(1)

path = sys.argv[1]
offset, limit, maxb = (int(x) for x in sys.argv[2:5])
offset = max(offset, 1)

if os.path.isdir(path):
    die("read_file FAILED: %s is a directory; use run_command with ls/find." % path)
if not os.path.isfile(path):
    die("read_file FAILED: %s does not exist." % path)
try:
    f = open(path, "rb")
except OSError as e:
    die("read_file FAILED: %s" % e)

chunks, used, total, last = [], 0, 0, offset - 1
why = None       # why output stopped early, if it did
note = None      # set when a single overlong line had to be cut
with f:
    if b"\0" in f.read(8192):
        die("read_file FAILED: %s looks binary; use run_command "
            "(e.g. file, od -c | head)." % path)
    f.seek(0)
    for n, line in enumerate(f, 1):
        total = n
        if n < offset or why:
            continue
        if limit and n >= offset + limit:
            why = "line limit"
            continue
        if used + len(line) > maxb:
            why = "byte limit"
            if not chunks:   # first line alone exceeds the budget: cut it
                part = line[:maxb].decode("utf-8", "ignore").encode("utf-8")
                chunks.append(part)
                note = "line %d is %d bytes; only the first %d are shown" % (
                    n, len(line), len(part))
                last = n
            continue
        chunks.append(line)
        used += len(line)
        last = n

if not total:
    out("%s: empty file" % path)
    sys.exit(0)
if offset > total:
    die("read_file FAILED: offset %d is past the end (%s has %d lines)."
        % (offset, path, total))

out("%s: lines %d-%d of %d" % (path, offset, last, total))
sys.stdout.flush()
body = b"".join(chunks)
sys.stdout.buffer.write(body)
if not body.endswith(b"\n"):
    sys.stdout.buffer.write(b"\n")   # no final newline, or a cut line
sys.stdout.buffer.flush()
if note:
    out("…[%s]…" % note)
if why and last < total:
    out("…[stopped at %s; %d more lines — continue with offset=%d]…"
        % (why, total - last, last + 1))
