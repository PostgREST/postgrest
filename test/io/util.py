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
