import contextlib
import socket
import threading

import jwt


class Thread(threading.Thread):
    "Variant of threading.Thread that re-raises any exceptions when joining the thread"

    def __init__(self, *args, **kwargs):
        self._exception = None
        super(Thread, self).__init__(*args, **kwargs)

    def run(self):
        try:
            super(Thread, self).run()
        except Exception as e:
            self._exception = e

    def join(self):
        super(Thread, self).join()
        if self._exception is not None:
            raise self._exception


def freeport():
    "Find a free port on localhost."
    with contextlib.closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(("", 0))
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return s.getsockname()[1]


def authheader(token):
    "Bearer token HTTP authorization header."
    return {"Authorization": f"Bearer {token}"}


def jwtauthheader(claim, secret):
    "Authorization header with signed JWT."
    return authheader(jwt.encode(claim, secret))


def parse_server_timings_header(header):
    """Parse the Server-Timing header into a dict of metric names to values.

    The header is a comma-separated list of metrics, each of which has a name
    and a duration. The duration may be followed by a semicolon and a list of
    parameters, but we ignore those.

    See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Server-Timing
    """
    timings = {}
    for timing in header.split(","):
        name, duration_text, *_ = timing.split(";")
        _, duration = duration_text.split("=")
        timings[name.strip()] = float(duration)
    return timings
