import datetime
from functools import partial
import random
import shutil
import time
import inspect
import argparse

from game import Game, GameOver
from solver import Solver

log_dir = "./logs/"

game_id = f"{datetime.datetime.now().strftime('%Y-%m-%d---%H-%M-%S---%f')}---{random.randint(1, 100)}"
log_file_path = f"{log_dir}/games/{game_id}.game"

parser = argparse.ArgumentParser()
parser.add_argument("-H", "--headless", action="store_true", help="Run without browser gui")
parser.add_argument("-l", "--level", choices=["easy", "medium", "hard"], default="hard", help="Minesweeper level")
parser.add_argument("-m", "--mute", action="store_true", help="Mute game sounds")
parser.add_argument("-s", "--skip-flags", action="store_true", help="Don't mark flags in gui")
args = parser.parse_args()


with open(log_file_path, "w") as log_file:
    print("Starting", log_file_path)
    log = partial(print, file=log_file, flush=True)

    game = Game(args.level, show_flags=not args.skip_flags, headless=args.headless, mute=args.mute, log=log)
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
                    ss_path = f"{log_dir}/unsolved_screenshots/{game_id}.png"
                    if not game.show_flags:
                        game.flag_all()
                    game.save_screenshot(ss_path)
                    log(f"Screenshot at {ss_path}")
                    shutil.copy2(log_file_path, f"{log_dir}/unsolved/")
                    break
    except GameOver:
        log("Game Over")
        end = time.time()
        log(f"Took {round(end-start)}s")
