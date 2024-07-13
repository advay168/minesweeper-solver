from abc import ABC, abstractmethod

from game import Pos


class Solver(ABC):
    @abstractmethod
    def next_pos(self) -> tuple[bool, Pos] | None:
        pass
