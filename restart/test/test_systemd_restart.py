import os
import subprocess
import time

import pytest
import requests

PORT = int(os.environ["PORT"])
SERVICE = os.environ["SERVICE"]


@pytest.fixture(scope="module", autouse=True)
def service_started():
    start_service()
    wait_for_app()

    yield

    subprocess.run(["systemctl", "stop", SERVICE], check=False)


def request(path):
    response = requests.get(f"http://127.0.0.1:{PORT}{path}", timeout=2)
    response.raise_for_status()

    return response.text.strip()


def main_pid():
    return subprocess.check_output(
        ["systemctl", "show", "-p", "MainPID", "--value", SERVICE],
        text=True,
    ).strip()


def app_pid():
    return request("/pid")


def start_service():
    subprocess.check_call(["systemctl", "start", SERVICE], timeout=10)


def reload_service():
    subprocess.check_call(["systemctl", "reload", SERVICE], timeout=10)


def wait_for_app():
    def app_responds():
        try:
            app_pid()
        except requests.RequestException:
            return False

        return True

    wait_until(app_responds)


def wait_until(predicate, timeout=10):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            if predicate():
                return
        except requests.RequestException:
            pass

        time.sleep(0.1)

    raise TimeoutError("condition did not become true")


def wait_for_handover(old_pid):
    wait_until(lambda: main_pid() != old_pid)
    new_pid = main_pid()
    wait_until(lambda: app_pid() == new_pid)
    subprocess.check_call(["systemctl", "is-active", "--quiet", SERVICE])

    return new_pid


def test_restart_replaces_main_process():
    old_pid = main_pid()
    assert old_pid == app_pid()
    assert request("/mode") == "standalone"

    reload_service()
    mid_pid = wait_for_handover(old_pid)
    assert mid_pid != old_pid

    reload_service()
    new_pid = wait_for_handover(mid_pid)
    assert new_pid != mid_pid
