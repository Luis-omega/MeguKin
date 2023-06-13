from dataclasses import dataclass
from abc import ABC, abstractmethod


@dataclass
class Task:
    """This class represent a task to be performed async"""

    info: None


@dataclass
class TaskManager(ABC):
    @abstractmethod
    def add_task(self, task: Task):
        pass
