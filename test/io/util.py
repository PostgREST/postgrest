import threading
import jwt
import os


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


def execute_sql_statement_using_superuser(env, statement):
    "Execute SQL statement with psql using superuser"

    superuser = "postgres"
    os.system(
        f'psql -d {env["PGDATABASE"]} -U {superuser} -h {env["PGHOST"]} --set ON_ERROR_STOP=1 -a -c "{statement}"'
    )
