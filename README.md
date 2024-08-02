# Minesweeper Solver

Solves [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_(video_game)) using an automated browser and custom constraint based solver.

## Features
- Currently has one frontend and two backends:
  - (Frontend) Google builtin minesweeper game automated with selenium (can also be run `--headless`)
  - (Backend) (default, `--solver py`) Adhoc constraint based solver written in python
  - (Backend) (`--solver hs`) Generalised constraint based solver written in haskell (with psuedo proofs for its moves)
- Saves log files for each game with detailed information for each action (to `./logs/`)
- Written in modular manner to allow adding additional frontends and backends with ease

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
- Run with default settings
```
python src/main.py
```
- View help for available options
```
python src/main.py -h
```
- Run program indefinitely (only windows script available for now)
```
run_infinite.bat {args to forward to main.py}
```
