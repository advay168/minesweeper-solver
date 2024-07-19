import argparse
import datetime
import functools
import pathlib
import random
import shutil
import time

from adhoc_python_solver import AdhocPythonSolver
from game import Game, GameOver
from google_selenium_game import GoogleSeleniumGame
from haskell_solver import HaskellSolver
from solver import Solver


def run(game: Game, solver: Solver, handle_unsolved, log, check: bool):
    start = time.time()
    game.start()
    game.update()
    try:
        while True:
            log()
            log(game)
            match solver.next_pos():
                case flag, pos:
                    if flag:
                        log(f"FLAG at {pos}")
                        game.flag(pos)
                    else:
                        log(f"OPEN at {pos}")
                        game.open(pos)
                    if check:
                        game.update()
                case None:
                    if game.update():
                        continue

                    handle_unsolved()
                    return
    except GameOver:
        end = time.time()
        log("Game Over")
        log(f"Took {round(end-start)}s")
        print("Game Over")
        print(f"Took {round(end-start)}s")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-H", "--headless", action="store_true", help="Run without browser gui"
    )
    parser.add_argument(
        "--solver",
        choices=["py", "hs"],
        default="py",
        help="Choose solver",
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


    parser.add_argument("-c", "--check", action="store_true", help="Double check each move")

    args = parser.parse_args()

    game_id = f"{datetime.datetime.now().strftime('%Y-%m-%d---%H-%M-%S---%f')}---{random.randint(1, 100)}"

    log_dir = pathlib.Path(__file__).parent / "logs/"

    log_file_path = log_dir / "games" / f"{game_id}.game"
    log_file_path.parent.mkdir(exist_ok=True, parents=True)

    def handle_unsolved():
        unsolved_ss_path = log_dir / "unsolved_screenshots" / f"{game_id}.png"
        unsolved_ss_path.parent.mkdir(exist_ok=True, parents=True)

        unsolved_dir = log_dir / "unsolved/"
        unsolved_dir.mkdir(exist_ok=True, parents=True)

        print("Not solveable")
        game.save_screenshot(unsolved_ss_path)
        log(f"Screenshot at {unsolved_ss_path}")
        shutil.copy2(log_file_path, unsolved_dir)

    with log_file_path.open("w") as log_file:
        print("Starting", log_file_path)
        log = functools.partial(print, file=log_file, flush=True)

        game = GoogleSeleniumGame(
            args.level,
            show_flags=not args.skip_flags,
            headless=args.headless,
            mute=args.mute,
            log=log,
        )

        if args.solver == "py":
            solver = AdhocPythonSolver(game=game, log=log)
        else:
            solver = HaskellSolver(game=game, log=log)

        solver_src = solver.get_source()
        log("---<Solver Code>---")
        log(solver_src)
        log("---</Solver Code>---")

        run(game, solver, handle_unsolved, log, args.check)


if __name__ == "__main__":
    main()
