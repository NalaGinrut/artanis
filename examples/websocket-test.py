#!/usr/bin/env python3
# Raw-socket end-to-end checks for the Artanis WebSocket layers 2 and 4.
# Needs the routes of examples/ENTRY.websocket, and server.websocket = true.
import base64, os, socket, struct, sys, time

HOST, PORT = "127.0.0.1", 3000
KEY = "dGhlIHNhbXBsZSBub25jZQ=="
results = []

def check(name, cond, info=""):
    results.append((name, bool(cond)))
    print(("PASS " if cond else "FAIL ") + name + ("" if cond else f"  -- {info}"))

def conn(timeout=5):
    s = socket.create_connection((HOST, PORT), timeout=timeout)
    return s

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

def upgrade_req(path="/echo", key=KEY, version="13", method="GET",
                httpver="1.1", extra="", upgrade=True, host=True):
    lines = [f"{method} {path} HTTP/{httpver}"]
    if host: lines.append("Host: localhost:3000")
    if upgrade:
        lines += ["Upgrade: websocket", "Connection: Upgrade"]
    if key is not None: lines.append(f"Sec-WebSocket-Key: {key}")
    if version is not None: lines.append(f"Sec-WebSocket-Version: {version}")
    if extra: lines.append(extra)
    return "\r\n".join(lines) + "\r\n\r\n"

def ws_open(path="/echo", extra=""):
    s = conn()
    head, rest = http_raw(s, upgrade_req(path, extra=extra))
    return s, head, rest

def frame(opcode, payload=b"", fin=True, mask=True, rsv=0):
    b0 = (0x80 if fin else 0) | (rsv << 4) | opcode
    n = len(payload)
    if n < 126: hdr = struct.pack("!BB", b0, (0x80 if mask else 0) | n)
    elif n < 65536: hdr = struct.pack("!BBH", b0, (0x80 if mask else 0) | 126, n)
    else: hdr = struct.pack("!BBQ", b0, (0x80 if mask else 0) | 127, n)
    if not mask:
        return hdr + payload
    m = os.urandom(4)
    return hdr + m + bytes(c ^ m[i % 4] for i, c in enumerate(payload))

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
        assert not (b1 & 0x80), "server frame masked"
        return (b0 & 0x80 != 0, b0 & 0x0f, self.need(n))
    def eof(self, wait=3):
        # True if the peer closes the TCP connection within `wait` seconds.
        self.s.settimeout(wait)
        try:
            while True:
                d = self.s.recv(65536)
                if not d:
                    return True
                self.buf += d
        except socket.timeout:
            return False
        except ConnectionResetError:
            return True

def close_code(p):
    return struct.unpack("!H", p[:2])[0] if len(p) >= 2 else None

# ---------------------------------------------------------------------------
def t_http_still_works():
    s = conn()
    head, body = http_raw(s, "GET /hello HTTP/1.1\r\nHost: x\r\n\r\n")
    check("plain HTTP route still works", head.startswith("HTTP/1.1 200"), head)

def t_handshake_ok():
    s, head, rest = ws_open()
    check("101 on valid handshake", head.startswith("HTTP/1.1 101"), head)
    check("accept key", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" in head, head)
    check("no subprotocol unless asked", "sec-websocket-protocol" not in head.lower(), head)
    s.close()

def t_subprotocol():
    s, head, _ = ws_open("/room/7", extra="Sec-WebSocket-Protocol: foo, chat")
    check("subprotocol selected when requested", "sec-websocket-protocol: chat" in head.lower(), head)
    s.close()
    s, head, _ = ws_open("/room/7", extra="Sec-WebSocket-Protocol: foo")
    check("unknown subprotocol: 101 without header",
          head.startswith("HTTP/1.1 101") and "sec-websocket-protocol" not in head.lower(), head)
    s.close()

def reject(name, req, status, must=()):
    s = conn()
    head, _ = http_raw(s, req)
    ok = head.startswith(f"HTTP/1.1 {status}") and all(m.lower() in head.lower() for m in must)
    check(name, ok, head)
    r = Reader(s)
    check(name + ": connection closed", r.eof())

def t_rejects():
    reject("not an upgrade -> 426", upgrade_req(upgrade=False), 426,
           ["upgrade: websocket", "sec-websocket-version: 13"])
    reject("bad version -> 426", upgrade_req(version="8"), 426, ["sec-websocket-version: 13"])
    reject("no version -> 400", upgrade_req(version=None), 400)
    reject("no key -> 400", upgrade_req(key=None), 400)
    reject("bad key -> 400", upgrade_req(key="abc"), 400)
    reject("key not 16 bytes -> 400", upgrade_req(key=base64.b64encode(b"x" * 15).decode()), 400)
    reject("POST -> 400", upgrade_req(method="POST"), 400)
    reject("HTTP/1.0 -> 400", upgrade_req(httpver="1.0"), 400)
    s = conn()
    head, _ = http_raw(s, upgrade_req(upgrade=False, extra="Upgrade: WebSocket\r\nConnection: keep-alive, Upgrade"))
    check("case-insensitive Upgrade value accepted", head.startswith("HTTP/1.1 101"), head)
    s.close()

def t_echo():
    s, head, rest = ws_open()
    r = Reader(s, rest)
    s.sendall(frame(1, b"hello"))
    fin, op, p = r.frame()
    check("text echo", (fin, op, p) == (True, 1, b"hello"), (fin, op, p))
    s.sendall(frame(2, b"\x00\x01\x02"))
    fin, op, p = r.frame()
    check("binary echo keeps type", (op, p) == (2, b"\x00\x01\x02"), (op, p))
    # fragmented with a ping in between
    s.sendall(frame(1, b"frag", fin=False) + frame(9, b"pp") + frame(0, b"ment"))
    f1 = r.frame(); f2 = r.frame()
    check("ping between fragments -> pong", f1[1] == 10 and f1[2] == b"pp", f1)
    check("fragmented message reassembled", f2[1:] == (1, b"fragment"), f2)
    # several messages in one segment
    s.sendall(frame(1, b"a") + frame(1, b"b") + frame(1, b"c"))
    got = [r.frame()[2] for _ in range(3)]
    check("pipelined messages", got == [b"a", b"b", b"c"], got)
    # large message
    big = os.urandom(200000)
    s.sendall(frame(2, big))
    check("200KB binary echo", r.frame()[2] == big)
    # route keys still work
    s.close()
    s, head, rest = ws_open("/room/42")
    r = Reader(s, rest)
    s.sendall(frame(1, b"hi"))
    check("route keys in ws handler", r.frame()[2] == b"42:hi")
    s.close()

def t_first_message_in_handshake_segment():
    s = conn()
    s.sendall(upgrade_req().encode() + frame(1, b"early"))
    data = b""
    while b"\r\n\r\n" not in data:
        data += s.recv(4096)
    head, _, rest = data.partition(b"\r\n\r\n")
    r = Reader(s, rest)
    check("message sent with the handshake", r.frame()[2] == b"early")
    s.close()

def t_close_by_client():
    s, head, rest = ws_open()
    r = Reader(s, rest)
    s.sendall(frame(8, struct.pack("!H", 1000) + b"bye"))
    fin, op, p = r.frame()
    check("close echoed", op == 8 and close_code(p) == 1000, (op, p))
    t = time.time()
    check("TCP closed promptly after close", r.eof(2) and time.time() - t < 2)

def t_quiet():
    s, head, rest = ws_open("/quiet")
    r = Reader(s, rest)
    s.sendall(frame(1, b"x"))
    s.sendall(frame(9, b"ping"))
    fin, op, p = r.frame()
    check("no ws-send -> no reply (next frame is the pong)", op == 10 and p == b"ping", (op, p))
    s.close()

def expect_close(name, path, send, code, still_open_before=False):
    s, head, rest = ws_open(path)
    r = Reader(s, rest)
    s.sendall(send)
    try:
        fin, op, p = r.frame()
    except EOFError:
        check(name, False, "EOF without close frame"); return
    check(name, op == 8 and close_code(p) == code, (op, close_code(p) if op == 8 else p))
    check(name + ": TCP closed", r.eof(3))

def t_errors():
    expect_close("handler artanis-err 500 -> 1011", "/boom", frame(1, b"x"), 1011)
    expect_close("handler scheme error -> 1011", "/boom", frame(1, b"scheme"), 1011)
    expect_close("handler 404 -> 1008", "/boom", frame(1, b"404"), 1008)
    expect_close("unmasked frame -> 1002", "/echo", frame(1, b"x", mask=False), 1002)
    expect_close("rsv bits -> 1002", "/echo", frame(1, b"x", rsv=4), 1002)
    expect_close("invalid utf8 -> 1007", "/echo", frame(1, b"\xff\xfe"), 1007)
    expect_close("bad close code -> 1002", "/echo", frame(8, struct.pack("!H", 999)), 1002)
    expect_close("continuation without start -> 1002", "/echo", frame(0, b"x"), 1002)

def t_server_alive():
    t_http_still_works()
    s, head, rest = ws_open()
    r = Reader(s, rest)
    s.sendall(frame(1, b"alive"))
    check("server still serves ws", r.frame()[2] == b"alive")
    s.close()

def t_abrupt_disconnects():
    # peer drops mid-frame and right after handshake; server must survive
    for _ in range(20):
        s, head, rest = ws_open()
        s.sendall(frame(1, b"hello world")[:5])
        s.close()
        s, head, rest = ws_open()
        s.close()
    time.sleep(0.5)
    t_server_alive()

def t_fd_reuse():
    # A closed ws connection's fd is reused by plain HTTP: it must be HTTP.
    for _ in range(30):
        s, head, rest = ws_open()
        r = Reader(s, rest)
        s.sendall(frame(8, struct.pack("!H", 1000)))
        r.frame(); r.eof(2); s.close()
        s = conn()
        head, body = http_raw(s, "GET /hello HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")
        if not head.startswith("HTTP/1.1 200"):
            check("fd reuse: HTTP after ws close", False, head); return
        s.close()
    check("fd reuse: HTTP after ws close", True)

def t_idle_timeout():
    s, head, rest = ws_open("/short")   # #:timeout 2
    r = Reader(s, rest)
    t = time.time()
    s.settimeout(10)
    try:
        fin, op, p = r.frame()
    except Exception as e:
        check("idle timeout -> close 1001", False, repr(e)); return
    dt = time.time() - t
    check("idle timeout -> close 1001", op == 8 and close_code(p) == 1001, (op, p))
    check(f"idle timeout within [T-1, 2T+1]s (took {dt:.1f}s)", 1 <= dt <= 5.5, dt)
    check("idle timeout: TCP closed", r.eof(3))

def t_activity_keeps_alive():
    s, head, rest = ws_open("/short")
    r = Reader(s, rest)
    ok = True
    for i in range(6):
        time.sleep(1)
        s.sendall(frame(1, b"k"))
        f = r.frame()
        ok = ok and f[1] == 1
    check("activity keeps an idle-timeout connection alive (6s > 2T)", ok)
    s.close()


def t_conformance_extra():
    expect_close("control frame > 125 -> 1002", "/echo", frame(9, b"x" * 126), 1002)
    expect_close("fragmented ping -> 1002", "/echo", frame(9, b"x", fin=False), 1002)
    expect_close("close with 1-byte payload -> 1002", "/echo", frame(8, b"\x03"), 1002)
    expect_close("close reason invalid utf8 -> 1007", "/echo",
                 frame(8, struct.pack("!H", 1000) + b"\xff"), 1007)
    # Only the header: a payload still in flight would be reset by the TCP
    # close (see the handoff, lingering close is not done yet).
    expect_close("frame over maxpayload -> 1009", "/echo",
                 struct.pack("!BBQ", 0x82, 0x80 | 127, (1 << 20) + 1) + b"mask", 1009)
    expect_close("new message inside fragmented one -> 1002", "/echo",
                 frame(1, b"a", fin=False) + frame(1, b"b"), 1002)
    # utf8 code point split across fragments is fine
    s, head, rest = ws_open()
    r = Reader(s, rest)
    e = "\u00e9".encode()
    s.sendall(frame(1, e[:1], fin=False) + frame(0, e[1:]))
    check("utf8 split across fragments", r.frame()[2] == e)
    # ping with 125 bytes, unsolicited pong ignored
    s.sendall(frame(10, b"unsolicited") + frame(9, b"p" * 125))
    f = r.frame()
    check("125-byte ping -> pong, unsolicited pong ignored", f[1] == 10 and f[2] == b"p" * 125, f[:2])
    # empty text message is echoed as an empty message
    s.sendall(frame(1, b"") + frame(1, b"after"))
    check("empty message echoed", r.frame()[1:] == (1, b""))
    check("... next is served", r.frame()[2] == b"after")
    # close with no payload
    s.sendall(frame(8))
    f = r.frame()
    check("empty close echoed", f[1] == 8 and f[2] == b"", f)
    check("empty close: TCP closed", r.eof(2))


def t_named_pipe_replace():
    s1, h1, r1 = ws_open("/echo?artanis_named_pipe=np1")
    a = Reader(s1, r1)
    s1.sendall(frame(1, b"one")); a.frame()
    s2, h2, r2 = ws_open("/echo?artanis_named_pipe=np1")
    check("second pipe connection accepted", h2.startswith("HTTP/1.1 101"), h2)
    f = a.frame()
    check("replaced pipe connection gets close 1001", f[1] == 8 and close_code(f[2]) == 1001, f)
    check("replaced pipe connection: TCP closed", a.eof(2))
    b = Reader(s2, r2)
    s2.sendall(frame(1, b"two"))
    check("new pipe connection works", b.frame()[2] == b"two")
    s2.close()

def t_half_frame_doesnt_block():
    s1, h1, r1 = ws_open()
    slow = frame(1, b"slow client")
    s1.sendall(slow[:4])       # half a frame, then stall
    s2, h2, r2 = ws_open()
    b = Reader(s2, r2)
    s2.sendall(frame(1, b"fast"))
    check("half frame of one client doesn't block others", b.frame()[2] == b"fast")
    s3 = conn()
    head, _ = http_raw(s3, "GET /hello HTTP/1.1\r\nHost: x\r\n\r\n")
    check("... nor HTTP", head.startswith("HTTP/1.1 200"), head)
    s1.sendall(slow[4:])
    check("stalled client resumes", Reader(s1, r1).frame()[2] == b"slow client")
    s1.close(); s2.close()

# ---------------------------------------------------------------------------
# Layer 4: the connection API

def http_get(path):
    s = conn()
    head, body = http_raw(s, f"GET {path} HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")
    # read the rest of the body
    s.settimeout(5)
    try:
        while True:
            d = s.recv(65536)
            if not d:
                break
            body += d
    except socket.timeout:
        pass
    s.close()
    return head, body.decode(errors="replace")

def closed_as(name, wait=3):
    # on-close of the test routes records "code reason" for /closed/NAME
    t = time.time()
    while True:
        _, body = http_get(f"/closed/{name}")
        if body != "open" or time.time() - t > wait:
            return body
        time.sleep(0.1)

def t_push():
    s, head, rest = ws_open("/sub?sub=p1")
    r = Reader(s, rest)
    _, body = http_get("/push/p1?msg=hi")
    check("push from HTTP: ws-send -> #t", body == "#t", body)
    check("push from HTTP arrives while waiting for the peer", r.frame() == (True, 1, b"hi"))
    _, body = http_get("/runner-push/p1")
    check("push from a runner thread: ws-send -> #t", body == "#t", body)
    check("push from a runner thread arrives", r.frame()[1:] == (1, b"from runner"))
    s.sendall(frame(1, b"still echo"))
    check("echo after pushes", r.frame()[2] == b"still echo")
    _, body = http_get("/push/nobody")
    check("push to an unknown name", body == "none", body)
    s.close()

def t_welcome():
    s, head, rest = ws_open("/welcome")
    r = Reader(s, rest)
    check("on-open sends before any message", r.frame()[1:] == (1, b"welcome"))
    s.sendall(frame(1, b"x"))
    check("... then on-message", r.frame()[2] == b"x")
    s.close()

def t_invalid_handlers():
    expect_close("handler returns a non-dispatcher -> 1011", "/not-dispatcher", b"", 1011)
    expect_close("handler throws 403 -> 1008", "/handler-403", b"", 1008)

def t_server_close():
    s, head, rest = ws_open("/ctl?sub=c3")
    r = Reader(s, rest)
    s.sendall(frame(1, b"binary"))
    check("ws-buffer is sent as binary", r.frame()[1:] == (2, b"bin"))
    s.sendall(frame(1, b"text-buffer"))
    check("ws-buffer with #:type 'text", r.frame()[1:] == (1, b"txt"))
    s.sendall(frame(1, b"close"))
    check("queued message goes before ws-close!", r.frame()[1:] == (1, b"before close"))
    fin, op, p = r.frame()
    check("ws-close! sends its code and reason",
          op == 8 and close_code(p) == 4000 and p[2:] == b"bye", (op, p))
    check("ws-close!: TCP closed", r.eof(3))
    check("ws-send after ws-close! -> closed", closed_as("after-close") == "closed")
    check("on-close gets the code sent", closed_as("c3") == "4000 bye", closed_as("c3"))

def t_on_close_codes():
    s, head, rest = ws_open("/sub?sub=c1")
    r = Reader(s, rest)
    s.sendall(frame(8, struct.pack("!H", 1000) + b"see you"))
    r.frame(); r.eof(2); s.close()
    got = closed_as("c1")
    check("on-close: close frame of the peer", got == "1000 see you", got)
    s, head, rest = ws_open("/sub?sub=c2")
    s.close()
    got = closed_as("c2")
    check("on-close: peer gone without close -> 1006", got.startswith("1006"), got)
    s, head, rest = ws_open("/sub?sub=c4")
    r = Reader(s, rest)
    s.sendall(frame(8))
    r.frame(); r.eof(2); s.close()
    got = closed_as("c4")
    check("on-close: empty close frame -> 1005", got.startswith("1005"), got)

def t_idle_with_push():
    # Pushes from the server don't keep an idle connection alive.
    s, head, rest = ws_open("/sub-short?sub=ip")   # #:timeout 2
    r = Reader(s, rest)
    t = time.time()
    pushed = got = 0
    code = None
    s.settimeout(0.5)
    while time.time() - t < 8:
        _, body = http_get("/push/ip?msg=tick")
        if body == "#t":
            pushed += 1
        try:
            while True:
                fin, op, p = r.frame()
                if op == 8:
                    code = close_code(p); break
                got += 1
        except socket.timeout:
            pass
        except EOFError:
            break
        if code is not None:
            break
        time.sleep(0.3)
    dt = time.time() - t
    check("pushes arrive on an idle connection", got >= 1, (pushed, got))
    check(f"idle with pushes -> close 1001 (took {dt:.1f}s)", code == 1001 and dt <= 5.5, (code, dt))
    got = closed_as("ip")
    check("on-close: idle -> 1001", got.startswith("1001"), got)

def t_overflow_reject():
    # The HTTP handler runs to the end before the connection's task, so the
    # queue fills up: websocket.maxqueue (1 MiB) = 16 x 64 KiB.
    s, head, rest = ws_open("/sub?sub=o1")
    r = Reader(s, rest)
    _, body = http_get("/push/o1?n=17&size=65536")
    check("overflow: 16 queued, then 'overflow", body.split() == ["#t"] * 16 + ["overflow"], body)
    ok = all(r.frame()[1:] == (2, bytes(65536)) for _ in range(16))
    check("overflow reject: queued messages are delivered", ok)
    s.sendall(frame(1, b"alive"))
    check("overflow reject: connection stays open", r.frame()[2] == b"alive")
    s.close()

def t_overflow_close():
    s, head, rest = ws_open("/sub-slow?sub=o2")   # #:overflow close
    r = Reader(s, rest)
    _, body = http_get("/push/o2?n=18&size=65536")
    check("overflow close: 'overflow then 'closed",
          body.split() == ["#t"] * 16 + ["overflow", "closed"], body)
    fin, op, p = r.frame()
    check("overflow close: queue dropped, close 1008", op == 8 and close_code(p) == 1008, (op, p[:2]))
    check("overflow close: TCP closed", r.eof(3))
    got = closed_as("o2")
    check("on-close: overflow -> 1008", got.startswith("1008"), got)

def t_write_suspends_in_waiter():
    # The peer doesn't read: flushing the queue in the read waiter suspends
    # in the write. The server keeps serving others, and when the peer reads
    # again, the write goes on and then the read.
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4096)
    s.settimeout(10)
    s.connect((HOST, PORT))
    head, rest = http_raw(s, upgrade_req("/sub?sub=w1"))
    r = Reader(s, rest)
    _, body = http_get("/push/w1?n=16&size=65536")
    check("1 MiB queued for a peer that doesn't read", body.split() == ["#t"] * 16, body)
    time.sleep(0.5)
    head2, _ = http_get("/hello")
    check("server serves others while a write is suspended", head2.startswith("HTTP/1.1 200"), head2)
    s.sendall(frame(1, b"after the flood"))
    ok = all(r.frame()[1:] == (2, bytes(65536)) for _ in range(16))
    check("suspended write resumes", ok)
    check("... then the read goes on", r.frame()[1:] == (1, b"after the flood"))
    s.close()

def t_runner_in_on_message():
    s, head, rest = ws_open("/slow")
    r = Reader(s, rest)
    t = time.time()
    s.sendall(frame(1, b"abc") + frame(9, b"pp"))
    head2, _ = http_get("/hello")
    check("server serves others during a runner", head2.startswith("HTTP/1.1 200"), head2)
    f1 = r.frame(); f2 = r.frame()
    check("runner result sent when on-message returns", f1[1:] == (1, b"ABC"), f1)
    check("ping answered after the runner (no read during it)", f2[1:] == (10, b"pp"), f2)
    check(f"runner took its time ({time.time() - t:.2f}s)", time.time() - t >= 0.3)
    s.close()

tests = [t_named_pipe_replace, t_half_frame_doesnt_block, t_conformance_extra, t_http_still_works, t_handshake_ok, t_subprotocol, t_rejects, t_echo,
         t_first_message_in_handshake_segment, t_close_by_client, t_quiet,
         t_errors, t_abrupt_disconnects, t_fd_reuse, t_idle_timeout,
         t_activity_keeps_alive, t_push, t_welcome, t_invalid_handlers,
         t_server_close, t_on_close_codes, t_idle_with_push,
         t_overflow_reject, t_overflow_close, t_write_suspends_in_waiter,
         t_runner_in_on_message, t_server_alive]
only = sys.argv[1:]
for t in tests:
    if only and t.__name__ not in only:
        continue
    try:
        t()
    except Exception as e:
        check(t.__name__, False, repr(e))
print(f"\n{sum(ok for _, ok in results)}/{len(results)} passed")
