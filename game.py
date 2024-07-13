from abc import ABC, abstractmethod

Pos = tuple[int, int]


class GameOver(Exception):
    pass


class Game(ABC):
    @abstractmethod
    def get_board(self) -> dict[Pos, str]:
        pass

    @abstractmethod
    def start(self):
        pass

    @abstractmethod
    def update(self) -> bool:
        pass

    @abstractmethod
    def flag(self, pos: Pos):
        pass

    @abstractmethod
    def mine(self, pos: Pos):
        pass

    @abstractmethod
    def save_screenshot(self, path):
        pass

    @property
    @abstractmethod
    def num_flags(self) -> int:
        pass

    @property
    @abstractmethod
    def num_dimensions(self) -> Pos:
        pass
