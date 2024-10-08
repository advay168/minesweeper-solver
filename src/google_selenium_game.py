import io
import json
import time
import pathlib
import tkinter as tk

from PIL import Image, ImageTk
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
        options.add_argument("--start-maximized")
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

    @staticmethod
    def parse_rgb_tuple(n: str) -> tuple[int, int, int]:
        r, g, b = n.split(",")
        return int(r), int(g), int(b)

    def lookup(self, n: str) -> str | None:
        threshold = 3
        r, g, b = self.parse_rgb_tuple(n)
        h = "headless" if self.headless else "headful"
        for nn, val in self.lookup_table[h][self.level].items():
            rr, gg, bb = self.parse_rgb_tuple(nn)
            if (
                val != ""
                and abs(rr - r) <= threshold
                and abs(gg - g) <= threshold
                and abs(bb - b) <= threshold
            ):
                return val

    def _update(self):
        # Fuzz test different window sizes
        if False:
            self.driver.set_window_size(
                np.random.randint(self.cols * 35, 1500),
                np.random.randint(self.rows * 35, 900),
            )
            time.sleep(1.3)

        time.sleep(0.9)

        self._move_to(self.initial).perform()
        ss = self._get_board_screenshot()
        self.settled = False
        for x in range(self.cols):
            for y in range(self.rows):
                n, thumbnail = self._characterise(ss, x, y)
                if self.lookup(n) is None:
                    self._handle_missing(n, thumbnail)
                self._board[(x, y)] = str(self.lookup(n))

    def _handle_missing(self, n: str, thumbnail: np.ndarray):
        if not self.first_time and not self.settled:
            # Settle board
            self.settled = True
            time.sleep(1)
            a = self._get_board_screenshot()
            if self.lookup(self._characterise(a, *self.initial)[0]) is None:
                raise GameOver()

        # Check if lookup updated in between (occurs with simultaneous processes)
        self._reload_lookup()
        if self.lookup(n):
            return

        # Prompt user
        if classification := self._prompt(thumbnail):
            self._reload_lookup()
            h = "headless" if self.headless else "headful"
            self.lookup_table[h][self.level][n] = classification
            self._write_lookup()

    def _prompt(self, thumbnail):
        root = tk.Tk()
        root.title("Classify Cell")
        root.geometry("+20+20")

        photo = ImageTk.PhotoImage(Image.fromarray(thumbnail).resize((300, 300)))

        image_label = tk.Label(root, image=photo)
        image_label.grid(row=0, column=0, padx=10, pady=10)

        button_frame = tk.Frame(root)
        button_frame.grid(row=1, column=0, padx=10, pady=10)

        output = ""

        def on_click(b: str):
            nonlocal output
            output = b
            root.destroy()

        def quit():
            print("Quitting")
            root.destroy()
            raise SystemExit()

        buttons = {
            "0": (0, 0),
            "1": (0, 1),
            "2": (0, 2),
            "3": (0, 3),
            "4": (0, 4),
            "5": (0, 5),
            "6": (1, 0),
            "7": (1, 1),
            "8": (1, 2),
            "?": (1, 3),
            "F": (1, 4),
        }

        for b, (row, col) in buttons.items():
            button = tk.Button(button_frame, text=b, command=lambda b=b: on_click(b))
            button.grid(row=row, column=col, pady=5, ipadx=10, ipady=10)

        button = tk.Button(button_frame, text="Quit", command=quit)
        button.grid(row=1, column=5, pady=5, ipady=10)

        root.mainloop()
        return output

    def _characterise(self, img: np.ndarray, x: int, y: int) -> tuple[str, np.ndarray]:
        pad = 3
        roi = img[
            y * self.cell_h + pad : (y + 1) * self.cell_h - pad,
            x * self.cell_w + pad : (x + 1) * self.cell_w - pad,
        ]
        r, g, b = roi.reshape(-1, 3).mean(axis=0).astype(int)
        return f"{r},{g},{b}", roi

    def _reload_lookup(self):
        with open(self.lookup_file_path) as f:
            self.lookup_table = json.load(f)

    def _write_lookup(self):
        with open(self.lookup_file_path, "w", newline="\n") as f:
            json.dump(self.lookup_table, f, indent=2)
