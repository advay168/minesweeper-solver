from abc import ABC, abstractmethod
from typing import Callable

Pos = tuple[int, int]


class GameOver(Exception):
    pass


class Game(ABC):
    def __init__(
        self,
        level: int,
        headless: bool,
        mute: bool,
        show_flags: bool,
        check: bool,
        log: Callable,
    ):
        self.level = level
        self.log = log
        self.headless = headless
        self.mute = mute
        self.show_flags = not headless and show_flags
        self.check = check

        self.first_time = True
        self.stale = False

    @property
    @abstractmethod
    def board(self) -> dict[Pos, str]:
        pass

    @abstractmethod
    def start(self):
        pass

    @abstractmethod
    def _update(self):
        pass

    def update(self) -> bool:
        if not self.stale:
            return False

        self.log("Updating")
        self.log("From:")
        self.log(self)

        prev = self.board.copy()

        self._update()
        board = self.board

        self.log("To:")
        self.log(self)

        for pos in prev:
            if prev[pos] != board[pos]:
                if not self.show_flags and prev[pos] == "F" and board[pos] == "?":
                    continue
                if prev[pos] == "?":
                    continue
                if prev[pos] == "#" and board[pos].isdigit():
                    continue
                else:
                    msg = f"GAME CORRUPTED!\nExpecting '{prev[pos]}' but found '{board[pos]}' at {pos}"
                    print(msg)
                    self.log(msg)
                    breakpoint()
                    raise SystemExit()

        if not self.show_flags:
            for pos, cell in prev.items():
                if cell == "F":
                    board[pos] = "F"

        self.stale = False
        self.first_time = False
        return True

    @abstractmethod
    def _flag(self, pos: Pos):
        pass

    def flag(self, pos: Pos):
        if self.board[pos] != "?":
            breakpoint()
        self.stale = True
        self.log("Flagging", pos)
        self.board[pos] = "F"
        if self.show_flags:
            self._flag(pos)
        if self.check:
            self.update()

    @abstractmethod
    def _open(self, pos: Pos):
        pass

    def open(self, pos: Pos):
        if self.board[pos] != "?":
            breakpoint()
        self.stale = True
        self.log("Opening", pos)
        self.board[pos] = "#"
        return self._open(pos)

    @abstractmethod
    def _save_screenshot(self, path):
        pass

    def save_screenshot(self, path):
        if not self.show_flags:
            for pos, v in self.board.items():
                if v == "F":
                    self.flag(pos)
        return self._save_screenshot(path)

    @property
    @abstractmethod
    def num_flags(self) -> int:
        pass

    @property
    @abstractmethod
    def dimensions(self) -> Pos:
        pass

    @property
    def initial(self) -> Pos:
        cols, rows = self.dimensions
        return (cols // 2, rows // 2)

    def __repr__(self):
        cols, rows = self.dimensions
        board = self.board
        s = ""
        s += "   " + ("0" * 10 + "1" * 10 + "2" * 10)[:cols] + " \n"
        s += "   " + ("0123456789" * 3)[:cols] + " \n"
        s += "  " + "-" * (cols + 2) + "\n"
        for y in range(rows):
            s += f"{y:02}|"
            for x in range(cols):
                c = board[(x, y)]
                s += c.translate(str.maketrans("?0", " ."))
            s += "|\n"
        s += "  " + "-" * (cols + 2)
        return s
