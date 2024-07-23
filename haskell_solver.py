from game import Game, Pos
from solver import Solver

import subprocess
import shutil
import pathlib
import threading


class HaskellSolver(Solver):
    def __init__(self, game: Game, log):
        self.game = game
        self.log = log
        runner = ""
        if shutil.which("runghc"):
            runner = ["runghc", pathlib.Path(__file__).parent / "haskell_solver.hs"]
        elif shutil.which("runhaskell"):
            runner = ["runhaskell", pathlib.Path(__file__).parent / "haskell_solver.hs"]
        else:
            runner = [pathlib.Path(__file__).parent / "haskell_solver"]
        self.instance = subprocess.Popen(
            runner,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )

        # Don't let handler capture `self` so browser closes gracefully
        haskell_logfile = self.instance.stderr

        def f():
            assert haskell_logfile
            for line in iter(haskell_logfile):
                line = line.decode("ascii").strip()
                log(line)

        self.logger = threading.Thread(target=f, daemon=True).start()

    def next_pos(self) -> tuple[bool, Pos] | None:
        assert self.instance.stdin
        assert self.instance.stdout

        board = self.game.board
        serialised_board = str(list(board.items()))
        self.instance.stdin.write(str(self.game.num_flags).encode("ascii"))
        self.instance.stdin.write(b" ")
        self.instance.stdin.write(serialised_board.encode("ascii"))
        self.instance.stdin.write(b"\n")
        self.instance.stdin.flush()

        output = self.instance.stdout.readline().decode("ascii").strip()
        match output.split():
            case ["F", p]:
                return True, eval(p)
            case ["O", p]:
                return False, eval(p)
            case ["?"]:
                return None
            case c:
                raise Exception(f"Unknown output {c}")

    def get_source(self) -> str:
        return (pathlib.Path(__file__).parent / "haskell_solver.hs").read_text()
