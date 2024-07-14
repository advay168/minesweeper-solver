import io
import json
import time
import pathlib

from PIL import Image
import matplotlib.pyplot as plt
import numpy as np
from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By

from game import Game, GameOver


class GoogleSeleniumGame(Game):
    def __init__(
        self,
        level="hard",
        headless=False,
        mute=False,
        show_flags=False,
        log=print,
        lookup_file_path=pathlib.Path(__file__).parent / "lookup.json",
    ):
        self.init = True
        self.stale = False

        self.level = level
        self.log = log
        self.show_flags = not headless and show_flags
        self.lookup_file_path = lookup_file_path

        options = webdriver.ChromeOptions()
        if headless:
            options.add_argument("--headless")
            options.add_argument("--remote-allow-origins=*")

        if mute:
            options.add_argument("--mute-audio")

        self.driver = webdriver.Chrome(options=options)
        self.driver.get("http://www.google.com/search?q=minesweeper")

        self.driver.find_element(By.XPATH, "//*[contains(text(), 'Play')]").click()
        self.canvas = self.driver.find_element(By.TAG_NAME, "canvas")
        time.sleep(0.5)

        match level:
            case "medium":
                self.cols = 18
                self.rows = 14
                self.flags = 40
            case "hard":
                self.cols = 24
                self.rows = 20
                self.flags = 99
                self.driver.find_element(
                    By.XPATH, "//div[@aria-expanded][contains(., 'Medium')]"
                ).click()
                self.driver.find_element(
                    By.XPATH, "//*[@data-difficulty='HARD']"
                ).click()
            case "easy":
                self.cols = 10
                self.rows = 8
                self.flags = 10
                self.driver.find_element(
                    By.XPATH, "//div[@aria-expanded][contains(., 'Medium')]"
                ).click()
                self.driver.find_element(
                    By.XPATH, "//*[@data-difficulty='EASY']"
                ).click()

        self.initial = (self.cols // 2, self.rows // 2)

        self._board = {(x, y): "?" for x in range(self.cols) for y in range(self.rows)}

        self.reload_canvas_dims()
        self.reload_lookup()

    def get_board(self):
        return self._board

    @property
    def num_flags(self):
        return self.flags

    @property
    def num_dimensions(self):
        return (self.cols, self.rows)

    def start(self):
        self.open(self.initial)
        time.sleep(1.5)

    def flag(self, pos):
        if self._board[pos] != "?":
            breakpoint()
        self.stale = True
        self.log("Flagging", pos)
        self._board[pos] = "F"
        if self.show_flags:
            self.move_to(pos).context_click().perform()

    def open(self, pos):
        if self._board[pos] != "?":
            breakpoint()
        self.stale = True
        self.log("Opening", pos)
        self._board[pos] = "#"
        self.move_to(pos).click().perform()

    def move_to(self, pos):
        x, y = pos
        return ActionChains(self.driver).move_to_element_with_offset(
            self.canvas,
            -self.canvas.size["width"] / 2 + self.cell_w * (x + 0.5),
            -self.canvas.size["height"] / 2 + self.cell_h * (y + 0.5),
        )

    def save_screenshot(self, path):
        if not self.show_flags:
            for pos, v in self._board.items():
                if v == "F":
                    self.move_to(pos).context_click().perform()
        Image.open(io.BytesIO(self.canvas.screenshot_as_png)).save(path)

    def reload_canvas_dims(self):
        self.cell_w = self.canvas.size["width"] // self.cols
        self.cell_h = self.canvas.size["height"] // self.rows

    def get_board_screenshot(self):
        self.reload_canvas_dims()
        return np.array(Image.open(io.BytesIO(self.canvas.screenshot_as_png)))

    def update(self):
        if not self.stale:
            return False
        time.sleep(0.9)

        self.log("Updating")
        self.log("From:")
        self.log(self)

        prev = self._board.copy()

        self.move_to(self.initial).perform()
        ss = self.get_board_screenshot()
        lut = self.lookup[self.level]
        for x in range(self.cols):
            for y in range(self.rows):
                n, thumbnail = self.characterise(ss, x, y)
                if n in lut and lut[n] != "":
                    self._board[(x, y)] = self.lookup[self.level][n]
                else:
                    if not self.init:
                        # Settle board
                        time.sleep(1)
                        a = self.get_board_screenshot()
                        if self.characterise(a, *self.initial)[0] not in lut:
                            raise GameOver()

                    # Prompt user
                    self.reload_lookup()
                    if n in lut and lut[n] != "":
                        return self.update()
                    self.lookup[self.level][n] = ""
                    self.write_lookup()
                    plt.imshow(thumbnail)
                    plt.title(n)
                    if 0:
                        plt.show(block=False)
                        plt.pause(7)
                        plt.close()
                    else:
                        plt.show()

                    self.reload_lookup()
                    return self.update()

        self.log("To:")
        self.log(self)

        for pos in prev:
            if prev[pos] != self._board[pos]:
                if not self.show_flags and prev[pos] == "F" and self._board[pos] == "?":
                    continue
                if prev[pos] == "?":
                    continue
                if prev[pos] == "#" and self._board[pos].isdigit():
                    continue
                else:
                    msg = f"GAME CORRUPTED!\nExpecting '{prev[pos]}' but found '{self._board[pos]}' at {pos}"
                    print(msg)
                    self.log(msg)
                    breakpoint()
                    raise SystemExit()

        self.init = False
        self.stale = False
        return True

    def characterise(self, img, x, y):
        pad = 7
        roi = img[
            y * self.cell_h + pad : (y + 1) * self.cell_h - pad,
            x * self.cell_w + pad : (x + 1) * self.cell_w - pad,
        ]
        r, g, b = roi.reshape(-1, 3).mean(axis=0).astype(int)
        return str(r * 256 * 256 + g * 256 + b), roi

    def __repr__(self):
        s = "-" * (self.cols + 2) + "\n"
        for y in range(self.rows):
            s += "|"
            for x in range(self.cols):
                c = self._board[(x, y)]
                s += c.translate(str.maketrans("?0", " ."))
            s += "|\n"
        s += "-" * (self.cols + 2)
        return s

    def reload_lookup(self):
        with open(self.lookup_file_path) as f:
            self.lookup = json.load(f)

    def write_lookup(self):
        with open(self.lookup_file_path, "w", newline="\n") as f:
            json.dump(self.lookup, f, indent=2)
