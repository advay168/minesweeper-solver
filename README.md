# Minesweeper Solver

Solves [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_(video_game)) using an automated browser and custom constraint based solver.

## Demo

https://github.com/user-attachments/assets/f1b60385-13c7-4c36-961f-1cc79f9c2cec

> [!NOTE]  
> The demo video is slightly outdated. Append `--cli` to the command so that the GUI does not pop up.

## Features
- Currently has one frontend and two backends:
  - (Frontend) Google builtin minesweeper game automated with selenium (can also be run `--headless`)
  - (Backend) (default, `--solver py`) Adhoc constraint based solver written in python
  - (Backend) (`--solver hs`) Generalised constraint based solver written in haskell (with psuedo proofs for its moves)
- Saves log files for each game with detailed information for each action (to `./logs/`)
- Written in modular manner to allow adding additional frontends and backends with ease
- GUI+CLI using [Gooey](https://github.com/chriskiehl/Gooey) (pass `--cli` to use on command line)

## Requirements
- Python
- Chrome (Uses chrome specific features)
- [GHC](https://www.haskell.org/ghcup/) (for haskell backend)

## Build/Run Instructions
### TLDR
```
python src/main.py
```
### Full Instructions
- Clone repository
```
git clone https://github.com/advay168/minesweeper-solver
cd minesweeper-solver
```
- Optionally: Create virtual enviroment
```
python -m venv .venv
./.venv/Scripts/activate
# On Windows: .\.venv\Scripts\activate
```
- Install python requirements
```
pip install -r requirements.txt
```
- Run with default settings and GUI
```
python src/main.py
```
- View help for available options
```
python src/main.py -h
```
- Run program indefinitely (only a Windows script available for now)
```
run_infinite.bat {args to forward to main.py}
```

## Limitations
- Selneium is quite slow and is the bottleneck. Other frontends can be added such as using pupeeter or a custom ui.
- If the game is unsolveable then it aborts instead of guessing the most likely move using heuristics.

