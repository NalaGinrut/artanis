#!/usr/bin/env python3
# End-to-end checks for the WebSocket AuthN layer (layer 3).
# Needs the routes of examples/ENTRY.websocket, and in conf/artanis.conf:
#   server.websocket = true
#   cookie.expires = 4
#   server.origins = http://localhost:5173
import socket, struct, sys, time, os

HOST, PORT = "127.0.0.1", 3000
KEY = "dGhlIHNhbXBsZSBub25jZQ=="
results = []

def check(name, cond, info=""):
    results.append((name, bool(cond)))
    print(("PASS " if cond else "FAIL ") + name + ("" if cond else f"  -- {info}"))

def conn():
    return socket.create_connection((HOST, PORT), timeout=10)

def http_raw(s, req):
    s.sendall(req.encode())
    data = b""
    while b"\r\n\r\n" not in data:
        d = s.recv(4096)
        if not d:
            break
        data += d
    head, _, rest = data.partition(b"\r\n\r\n")
    return head.decode(errors="replace"), rest

def login():
    s = conn()
    head, _ = http_raw(s, "GET /login HTTP/1.1\r\nHost: localhost:3000\r\nConnection: close\r\n\r\n")
    s.close()
    for line in head.split("\r\n"):
        if line.lower().startswith("set-cookie:") and "sid=" in line:
            return line.split("sid=", 1)[1].split(";", 1)[0]
    return None

def logout(sid):
    s = conn()
    head, _ = http_raw(s, f"GET /logout HTTP/1.1\r\nHost: localhost:3000\r\nCookie: sid={sid}\r\nConnection: close\r\n\r\n")
    s.close()
    return head

def handshake(path="/secure", sid=None, origin=None, host="localhost:3000"):
    lines = [f"GET {path} HTTP/1.1", f"Host: {host}", "Upgrade: websocket",
             "Connection: Upgrade", f"Sec-WebSocket-Key: {KEY}",
             "Sec-WebSocket-Version: 13"]
    if sid: lines.append(f"Cookie: sid={sid}")
    if origin: lines.append(f"Origin: {origin}")
    s = conn()
    head, rest = http_raw(s, "\r\n".join(lines) + "\r\n\r\n")
    return s, head, rest

def frame(opcode, payload=b""):
    m = os.urandom(4)
    n = len(payload)
    assert n < 126
    return struct.pack("!BB", 0x80 | opcode, 0x80 | n) + m + bytes(c ^ m[i % 4] for i, c in enumerate(payload))

class Reader:
    def __init__(self, s, buf=b""):
        self.s, self.buf = s, buf
    def need(self, n):
        while len(self.buf) < n:
            d = self.s.recv(65536)
            if not d:
                raise EOFError
            self.buf += d
        r, self.buf = self.buf[:n], self.buf[n:]
        return r
    def frame(self):
        b0, b1 = self.need(2)
        n = b1 & 0x7f
        if n == 126: n = struct.unpack("!H", self.need(2))[0]
        elif n == 127: n = struct.unpack("!Q", self.need(8))[0]
        return (b0 & 0x0f, self.need(n))
    def eof(self, wait=3):
        self.s.settimeout(wait)
        try:
            while True:
                d = self.s.recv(65536)
                if not d:
                    return True
        except socket.timeout:
            return False
        except ConnectionResetError:
            return True

def status(head):
    return head.split(" ", 2)[1] if head.startswith("HTTP/") else head

def rejected(name, code, **kw):
    s, head, rest = handshake(**kw)
    check(f"{name} -> {code}", status(head) == str(code), head.split("\r\n")[0])
    check(f"{name}: no body", "content-length: 0" in head.lower() and rest == b"", head)
    check(f"{name}: connection closed", Reader(s, rest).eof())

def accepted(name, **kw):
    s, head, rest = handshake(**kw)
    ok = status(head) == "101"
    check(f"{name} -> 101", ok, head.split("\r\n")[0])
    return s, rest

# ---------------------------------------------------------------------------
def t_auth():
    rejected("no session", 401)
    rejected("unknown sid", 401, sid="0" * 32)
    sid = login()
    check("login gives a sid", sid, sid)
    s, rest = accepted("valid session", sid=sid)
    r = Reader(s, rest)
    s.sendall(frame(1, b"secret"))
    check("authenticated echo", r.frame() == (1, b"secret"))
    s.close()
    # A route without #:with-auth needs no session.
    s, rest = accepted("route without #:with-auth", path="/echo")
    s.close()

def t_origin():
    sid = login()
    s, rest = accepted("same origin", sid=sid, origin="http://localhost:3000"); s.close()
    s, rest = accepted("same origin, case and default port",
                       sid=sid, origin="HTTP://LOCALHOST:3000"); s.close()
    s, rest = accepted("Host without port, origin default port",
                       sid=sid, origin="https://example.com", host="example.com"); s.close()
    s, rest = accepted("trusted origin (server.origins)",
                       sid=sid, origin="http://localhost:5173"); s.close()
    s, rest = accepted("no Origin (non-browser)", sid=sid); s.close()
    rejected("cross origin", 403, sid=sid, origin="http://evil.example")
    rejected("same host, other port", 403, sid=sid, origin="http://localhost:3001")
    rejected("opaque origin null", 403, sid=sid, origin="null")
    # Origin is checked before AuthN: no session leaks through it
    rejected("cross origin without session", 403, origin="http://evil.example")

def t_skip_and_recheck():
    # cookie.expires = 4: the session lives 4s and is checked every 4s.
    sid = login()
    s, rest = accepted("session for recheck", sid=sid)
    r = Reader(s, rest)
    logout(sid)
    # Logged out, but messages don't go through #:with-auth, the connection
    # works until the next check.
    s.sendall(frame(1, b"still"))
    check("messages don't go through #:with-auth", r.frame() == (1, b"still"))
    t = time.time()
    s.settimeout(15)
    try:
        op, p = r.frame()
    except Exception as e:
        check("gone session -> close 1008", False, repr(e)); return
    dt = time.time() - t
    code = struct.unpack("!H", p[:2])[0] if op == 8 and len(p) >= 2 else None
    check("gone session -> close 1008", op == 8 and code == 1008, (op, p))
    check(f"closed within 2 periods, idle (took {dt:.1f}s)", dt <= 9, dt)
    check("gone session: TCP closed", r.eof(3))

def t_expired_session():
    # The session expires by itself (4s) while the peer is idle.
    sid = login()
    s, rest = accepted("session that will expire", sid=sid)
    r = Reader(s, rest)
    s.settimeout(15)
    t = time.time()
    try:
        op, p = r.frame()
    except Exception as e:
        check("expired session -> close 1008", False, repr(e)); return
    code = struct.unpack("!H", p[:2])[0] if op == 8 and len(p) >= 2 else None
    check("expired session -> close 1008", op == 8 and code == 1008, (op, p))
    check(f"... in time ({time.time() - t:.1f}s)", time.time() - t <= 9)
    s.close()

def t_active_peer_recheck():
    # A peer sending all the time is checked before its messages too.
    sid = login()
    s, rest = accepted("chatty session", sid=sid)
    r = Reader(s, rest)
    logout(sid)
    closed = False
    for i in range(20):
        time.sleep(0.5)
        try:
            s.sendall(frame(1, b"x"))
            op, p = r.frame()
        except Exception:
            break
        if op == 8:
            closed = struct.unpack("!H", p[:2])[0] == 1008
            break
    check("chatty peer with gone session -> close 1008", closed)
    s.close()

tests = [t_auth, t_origin, t_skip_and_recheck, t_expired_session, t_active_peer_recheck]
only = sys.argv[1:]
for t in tests:
    if only and t.__name__ not in only:
        continue
    try:
        t()
    except Exception as e:
        check(t.__name__, False, repr(e))
print(f"\n{sum(ok for _, ok in results)}/{len(results)} passed")
