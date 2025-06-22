import json
import socket

from typing_extensions import override

from approvaltests import Reporter


def send_tcp_socket(host: str, port: int, data: str) -> None:
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        sock.connect((host, port))
        sock.sendall(bytes(data, encoding="utf-8"))
    finally:
        sock.close()


class ReportToDiffEngineTray(Reporter):
    @override
    def report(self, received_path: str, approved_path: str) -> bool:
        payload = {
            "Type": "Move",
            "Temp": received_path,
            "Target": approved_path,
            "CanKill": False,
        }

        send_tcp_socket("localhost", 3492, json.dumps(payload))
