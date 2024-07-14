from game import Game, Pos
from solver import Solver

import subprocess
import shutil


class HaskellSolver(Solver):
    def __init__(self, game: Game | None, log):
        self.game = game
        self.log = log
        runner = ""
        if shutil.which("runghc"):
            runner = ["runghc", "haskell_solver.hs"]
        elif shutil.which("runhaskell"):
            runner = ["runhaskell", "haskell_solver.hs"]
        else:
            runner = ["haskell_solver"]
        self.instance = subprocess.Popen(
            runner,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )

    def next_pos(self, b={}) -> tuple[bool, Pos] | None:
        board = self.game.get_board() if self.game else b
        serialised_board = str(list(board.items()))
        assert self.instance.stdin
        self.instance.stdin.write(serialised_board.encode("ascii"))
        self.instance.stdin.write(b"\n")
        self.instance.stdin.flush()
        assert self.instance.stdout
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


s = HaskellSolver(None, None)
