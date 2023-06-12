import logging
import logging.config

from pathlib import Path

log_file = Path(__file__).parent.resolve().parent.resolve() / "logging.config"
print(log_file)


def get_logger(name: str):
    print(name)
    log = logging.getLogger(name)
    handler = logging.FileHandler(f"logs/{name}.log", mode="w")
    formatt = logging.Formatter(
        fmt="%(asctime)s - %(filename)s - %(funcName)s - %(lineno)s - %(levelname)s - %(message)s"
    )
    handler.setFormatter(formatt)
    log.addHandler(handler)
    log.setLevel(logging.DEBUG)
    return log
