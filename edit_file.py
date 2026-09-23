"""Backend for the `edit_file' tool; runs inside the sandbox.

Usage: python3 edit_file.py PATH MODE    (MODE is "one" or "all")
Stdin: "NOLD NNEW\\n" followed by OLD and NEW, NOLD/NNEW being byte counts.
"""
import sys, os, difflib

def out(s): sys.stdout.write(s + "\n")
def die(s): out(s); sys.exit(1)

# --- Input -------------------------------------------------------------------
# Byte-count framing lets OLD/NEW contain any characters without quoting;
# surrogateescape round-trips bytes that are not valid UTF-8.
path, mode = sys.argv[1], sys.argv[2]
blob = sys.stdin.buffer.read()
head, rest = blob.split(b"\n", 1)
n_old, n_new = (int(x) for x in head.split())
d = lambda b: b.decode("utf-8", "surrogateescape")
old, new = d(rest[:n_old]), d(rest[n_old:n_old + n_new])

if not old:
    die("edit_file FAILED: `old' is empty; use write_file to create a file.")
if not os.path.isfile(path):
    die("edit_file FAILED: %s does not exist; use write_file to create it." % path)
with open(path, "r", encoding="utf-8", errors="surrogateescape", newline="") as f:
    src = f.read()

# --- Line endings ------------------------------------------------------------
# Work in LF internally.  A consistently-CRLF file is converted back on write;
# a mixed-ending file is never normalised (the fuzzy passes still match it,
# since rstrip()/strip() ignore a trailing \r).
crlf = "\r\n" in src and src.count("\r\n") == src.count("\n")
if crlf:
    src = src.replace("\r\n", "\n")
old, new = old.replace("\r\n", "\n"), new.replace("\r\n", "\n")
SL = src.split("\n")

def indent(l): return l[:len(l) - len(l.lstrip())]

# Longest indentation shared by all non-blank lines.
def common(ls): return os.path.commonprefix([indent(l) for l in ls if l.strip()])

def finish(res, n, how):
    """Write the result, report success and show a (truncated) diff."""
    if res == src:
        die("edit_file: nothing to do — `new' is identical to `old'.")
    with open(path, "w", encoding="utf-8", errors="surrogateescape", newline="") as f:
        f.write(res.replace("\n", "\r\n") if crlf else res)
    diff = list(difflib.unified_diff(SL, res.split("\n"),
                                     "a/" + path, "b/" + path, lineterm="", n=2))
    if len(diff) > 80:
        diff = diff[:80] + ["...(diff truncated)..."]
    out("edit_file OK: %s, %d replacement(s) [%s]" % (path, n, how))
    out("\n".join(diff))
    sys.exit(0)

# --- 1. Exact substring match ------------------------------------------------
c = src.count(old)
if c == 1 or (c > 1 and mode == "all"):
    finish(src.replace(old, new, -1 if mode == "all" else 1), c, "exact")
if c > 1:
    # Ambiguous: report where each (non-overlapping, like str.count) match is.
    ls, i = [], src.find(old)
    while i >= 0:
        ls.append(src.count("\n", 0, i) + 1)
        i = src.find(old, i + len(old))
    die("edit_file FAILED: `old' matches %d times in %s (lines %s). Include more "
        "surrounding context so it is unique, or pass replace_all=true."
        % (c, path, ", ".join(map(str, ls))))

# --- 2. Whitespace-tolerant, line-based fallbacks ----------------------------
# A trailing newline in `old' yields a final empty element; drop it (and the
# one in `new') so the match does not demand an extra blank line.  An empty
# `new' becomes no lines at all, so the matched lines are deleted outright.
OL = old.split("\n")
NL = new.split("\n") if new else []
if len(OL) > 1 and OL[-1] == "":
    OL.pop()
    if NL and NL[-1] == "":
        NL.pop()

def windows(key):
    """Start indices of non-overlapping runs of file lines equal to OL under key."""
    ko, ks, n = [key(l) for l in OL], [key(l) for l in SL], len(OL)
    ws, i = [], 0
    while i <= len(ks) - n:
        if ks[i:i + n] == ko:
            ws.append(i)
            i += n                      # skip past the match: no overlaps
        else:
            i += 1
    return ws

def reindent(l, oi, si):
    """Swap `old''s base indent (oi) for the file's (si), keeping relative indent."""
    if not l.strip():
        return l
    return si + l[len(os.path.commonprefix([indent(l), oi])):]

for how, key in (("ignoring trailing whitespace", lambda l: l.rstrip()),
                 ("ignoring indentation", lambda l: l.strip())):
    ws = windows(key)
    if len(ws) == 1 or (ws and mode == "all"):
        res, oi = SL[:], common(OL)
        for i in reversed(ws):          # back to front keeps indices valid
            si = common(SL[i:i + len(OL)])
            res[i:i + len(OL)] = [reindent(l, oi, si) for l in NL]
        finish("\n".join(res), len(ws), "%s, at line %d" % (how, ws[0] + 1))
    if len(ws) > 1:
        die("edit_file FAILED: `old' matches %d places %s (lines %s); add more "
            "context or pass replace_all=true."
            % (len(ws), how, ", ".join(str(i + 1) for i in ws)))

# --- 3. Not found: point the model at the most similar lines -----------------
probe = next((l for l in OL if l.strip()), "").strip()
cand = sorted(((difflib.SequenceMatcher(None, l.strip(), probe).ratio(), i + 1, l)
               for i, l in enumerate(SL)), reverse=True)[:3]
msg = ["edit_file FAILED: `old' not found in %s (%d lines)." % (path, len(SL)),
       "Copy the text VERBATIM from the file (re-read it first); do not retype it."]
hits = ["  %5d: %s" % (i, l.rstrip("\r")[:200]) for r, i, l in cand if r > 0.5]
if hits:
    msg.append("Closest lines to the first line of `old':")
    msg += hits
msg.append("Re-read with: sed -n 'START,ENDp' %s" % path)
die("\n".join(msg))

