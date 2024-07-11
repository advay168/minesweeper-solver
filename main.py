import argparse
import datetime
from functools import partial
import inspect
import pathlib
import random
import shutil
import time

from game import Game, GameOver
from solver import Solver

game_id = f"{datetime.datetime.now().strftime('%Y-%m-%d---%H-%M-%S---%f')}---{random.randint(1, 100)}"

log_dir = pathlib.Path("./logs/")

log_file_path = log_dir / "games" / f"{game_id}.game"
log_file_path.parent.mkdir(exist_ok=True, parents=True)

unsolved_ss_path = log_dir / "unsolved_screenshots" / f"{game_id}.png"
unsolved_ss_path.parent.mkdir(exist_ok=True, parents=True)

unsolved_dir = log_dir / "unsolved/"
unsolved_dir.mkdir(exist_ok=True, parents=True)

parser = argparse.ArgumentParser()
parser.add_argument(
    "-H", "--headless", action="store_true", help="Run without browser gui"
)
parser.add_argument(
    "-l",
    "--level",
    choices=["easy", "medium", "hard"],
    default="hard",
    help="Minesweeper level",
)
parser.add_argument("-m", "--mute", action="store_true", help="Mute game sounds")
parser.add_argument(
    "-s", "--skip-flags", action="store_true", help="Don't mark flags in gui"
)
args = parser.parse_args()


with log_file_path.open("w") as log_file:
    print("Starting", log_file_path)
    log = partial(print, file=log_file, flush=True)

    game = Game(
        args.level,
        show_flags=not args.skip_flags,
        headless=args.headless,
        mute=args.mute,
        log=log,
    )
    solver = Solver(game, log)

    solver_src = inspect.getsource(Solver)
    log("---<Solver Code>---")
    log(solver_src)
    log("---</Solver Code>---")

    start = time.time()
    try:
        game.start()
        game.update()

        while True:
            log()
            log(game)
            match solver.next_pos():
                case flag, pos:
                    if flag:
                        log(f"FLAG at {pos}")
                        game.flag(pos)
                    else:
                        log(f"MINE at {pos}")
                        game.mine(pos)
                case None:
                    if game.update():
                        continue

                    print("Not solveable")
                    if not game.show_flags:
                        game.flag_all()
                    game.save_screenshot(unsolved_ss_path)
                    log(f"Screenshot at {unsolved_ss_path}")
                    shutil.copy2(log_file_path, unsolved_dir)
                    break
    except GameOver:
        log("Game Over")
        end = time.time()
        log(f"Took {round(end-start)}s")
