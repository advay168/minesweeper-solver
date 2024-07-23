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
        *,
        level,
        headless,
        mute,
        show_flags,
        check,
        log=print,
        lookup_file_path=pathlib.Path(__file__).parent / "lookup.json",
    ):
        super().__init__(
            level=level,
            headless=headless,
            mute=mute,
            show_flags=show_flags,
            check=check,
            log=log,
        )

        self.lookup_file_path = lookup_file_path

        options = webdriver.ChromeOptions()
        options.add_argument("--log-level=3")
        if self.headless:
            options.add_argument("--headless")
            options.add_argument("--remote-allow-origins=*")

        if self.mute:
            options.add_argument("--mute-audio")

        self.driver = webdriver.Chrome(options=options)
        self.driver.get("http://www.google.com/search?q=minesweeper")

        self.driver.find_element(By.XPATH, "//*[contains(text(), 'Play')]").click()
        self.canvas = self.driver.find_element(By.TAG_NAME, "canvas")
        time.sleep(0.5)

        match self.level:
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

        self._board = {(x, y): "?" for x in range(self.cols) for y in range(self.rows)}

        self._reload_canvas_dims()
        self._reload_lookup()

    @property
    def board(self):
        return self._board

    @property
    def num_flags(self):
        return self.flags

    @property
    def dimensions(self):
        return (self.cols, self.rows)

    def start(self):
        self.open(self.initial)
        time.sleep(1.5)

    def _flag(self, pos):
        self._move_to(pos).context_click().perform()

    def _open(self, pos):
        self._move_to(pos).click().perform()

    def _move_to(self, pos):
        x, y = pos
        return ActionChains(self.driver).move_to_element_with_offset(
            self.canvas,
            -self.canvas.size["width"] / 2 + self.cell_w * (x + 0.5),
            -self.canvas.size["height"] / 2 + self.cell_h * (y + 0.5),
        )

    def _save_screenshot(self, path):
        Image.open(io.BytesIO(self.canvas.screenshot_as_png)).save(path)

    def _reload_canvas_dims(self):
        self.cell_w = self.canvas.size["width"] // self.cols
        self.cell_h = self.canvas.size["height"] // self.rows

    def _get_board_screenshot(self):
        self._reload_canvas_dims()
        return np.array(Image.open(io.BytesIO(self.canvas.screenshot_as_png)))

    def _update(self):
        time.sleep(0.9)

        self._move_to(self.initial).perform()
        ss = self._get_board_screenshot()
        lut = self.lookup[self.level]
        for x in range(self.cols):
            for y in range(self.rows):
                n, thumbnail = self._characterise(ss, x, y)
                if n in lut and lut[n] != "":
                    self._board[(x, y)] = self.lookup[self.level][n]
                else:
                    if not self.first_time:
                        # Settle board
                        time.sleep(1)
                        a = self._get_board_screenshot()
                        if self._characterise(a, *self.initial)[0] not in lut:
                            raise GameOver()

                    # Check if lookup updated in between (occurs with simultaneous processes)
                    self._reload_lookup()
                    if n in lut and lut[n] != "":
                        return self._update()

                    # Prompt user
                    self.lookup[self.level][n] = ""
                    self._write_lookup()

                    plt.imshow(thumbnail)
                    plt.title(n)
                    plt.show()

                    self._reload_lookup()
                    return self._update()

    def _characterise(self, img, x, y):
        pad = 7
        roi = img[
            y * self.cell_h + pad : (y + 1) * self.cell_h - pad,
            x * self.cell_w + pad : (x + 1) * self.cell_w - pad,
        ]
        r, g, b = roi.reshape(-1, 3).mean(axis=0).astype(int)
        return str(r * 256 * 256 + g * 256 + b), roi

    def _reload_lookup(self):
        with open(self.lookup_file_path) as f:
            self.lookup = json.load(f)

    def _write_lookup(self):
        with open(self.lookup_file_path, "w", newline="\n") as f:
            json.dump(self.lookup, f, indent=2)
