Pos = tuple[int, int]


class Solver:
    numeric: dict[Pos, int]
    open_neighbours: dict[Pos, set[Pos]]
    no_flag_neighbours: dict[Pos, int]
    mutexes: dict[Pos, list[set[Pos]]]
    mutex_groups: list[set[Pos]]
    double_mutex_groups: list[set[Pos]]

    def __init__(self, game, log):
        self.game = game
        self.flags = game.flags
        self.log = log

    def next_pos(self) -> tuple[bool, Pos] | None:
        self.calc_info()
        return self.run_tactics()

    def calc_info(self):
        board = self.game.board
        self.numeric = {}
        for pos, v in board.items():
            if v.isdigit():
                self.numeric[pos] = int(v)

        self.no_flag_neighbours = {}
        self.open_neighbours = {}
        for pos in board:
            self.open_neighbours[pos] = set()
            self.no_flag_neighbours[pos] = 0
            for neighbour in self._neighbours(pos):
                if board[neighbour] == "F":
                    self.no_flag_neighbours[pos] += 1
                if board[neighbour] == "?":
                    self.open_neighbours[pos].add(neighbour)

        self.mutexes = {k: [] for k in board}
        self.double_mutexes = {k: [] for k in board}
        self.mutex_groups = []
        self.double_mutex_groups = []
        for pos, n in self.numeric.items():
            if n - self.no_flag_neighbours[pos] == 1:
                group = set()
                for possible in self.open_neighbours[pos]:
                    self.mutexes[possible].append(group)
                    group.add(possible)
                self.mutex_groups.append(group)
            if n - self.no_flag_neighbours[pos] == 2:
                group = set()
                for possible in self.open_neighbours[pos]:
                    self.double_mutexes[possible].append(group)
                    group.add(possible)
                self.double_mutex_groups.append(group)

    def run_tactics(self) -> tuple[bool, Pos] | None:
        tactics = [
            self.tactic_A,
            self.tactic_B,
            self.tactic_C,
            self.tactic_D,
            self.tactic_E,
            self.tactic_F,
            self.tactic_G,
            self.tactic_H,
            self.tactic_I,
            self.tactic_J,
            self.tactic_K,
        ]

        for tactic in tactics:
            if ret := tactic():
                return ret

    def tactic_A(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            if len(possibles) == n - self.no_flag_neighbours[pos]:
                for possible in possibles:
                    self.log(f"TACTIC A:{pos} {possibles} {n}: Flagging {possible}")
                    return True, possible

    def tactic_B(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            if n == self.no_flag_neighbours[pos]:
                for possible in possibles:
                    self.log(f"TACTIC B:{pos} {possibles} {n}: Mining {possible}")
                    return False, possible

    def tactic_C(self):
        for group1 in self.mutex_groups:
            for group2 in self.mutex_groups:
                if group1.issubset(group2):
                    for p in group2 - group1:
                        self.log(f"TACTIC C:{group2} {group1}: Mining {p}")
                        return False, p

    def tactic_D(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            n -= self.no_flag_neighbours[pos]
            groups: set[frozenset[Pos]] = set()
            for possible in possibles:
                for group in self.mutexes[possible]:
                    groups.add(frozenset(group))

            for mutexes in self._divide(list(groups)):
                ps = possibles - set().union(*mutexes)
                if n - len(mutexes) == len(ps):
                    for possible in ps:
                        self.log(f"TACTIC D:{pos} {ps} {mutexes}: Flagging {possible}")
                        return True, possible

    def tactic_E(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            n -= self.no_flag_neighbours[pos]
            groups: set[frozenset[Pos]] = set()
            for possible in possibles:
                for group in self.mutexes[possible]:
                    if group.issubset(possibles):
                        groups.add(frozenset(group))

            for mutexes in self._divide_disjoint(list(groups)):
                ps = possibles - set().union(*mutexes)
                if n - len(mutexes) == 0:
                    for possible in ps:
                        self.log(f"tactic E:{pos} {ps} {mutexes}: Mining {possible}")
                        return False, possible

    def tactic_F(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            n -= self.no_flag_neighbours[pos]
            groups: set[frozenset[Pos]] = set()
            for possible in possibles:
                for group in self.mutexes[possible]:
                    if group.issubset(possibles):
                        groups.add(frozenset(group))

            added = False
            for mutexes in self._divide_disjoint(list(groups)):
                ps = possibles - set().union(*mutexes)
                if n - len(mutexes) == 1:
                    if ps not in self.mutex_groups:
                        added = True
                        for p in ps:
                            self.mutexes[p].append(ps)
                        self.mutex_groups.append(ps)
                        self.log(f"TACTIC F:{pos} {mutexes}: {ps}")
            if added:
                return self.run_tactics()

    def tactic_G(self):
        for group1 in self.double_mutex_groups:
            for group2 in self.double_mutex_groups:
                if group1.issubset(group2):
                    for p in group2 - group1:
                        self.log(f"TACTIC G:{group2} {group1}: Mining {p}")
                        return False, p

    def tactic_H(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            n -= self.no_flag_neighbours[pos]
            groups: set[frozenset[Pos]] = set()
            for possible in possibles:
                for group in self.double_mutexes[possible]:
                    groups.add(frozenset(group))

            for double_mutexes in self._divide(list(groups)):
                ps = possibles - set().union(*double_mutexes)
                if n - 2 * len(double_mutexes) == len(ps):
                    for possible in ps:
                        self.log(
                            f"TACTIC H:{pos} {ps} {double_mutexes}: Flagging {possible}"
                        )
                        return True, possible

    def tactic_I(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            n -= self.no_flag_neighbours[pos]
            groups: set[frozenset[Pos]] = set()
            for possible in possibles:
                for group in self.double_mutexes[possible]:
                    if group.issubset(possibles):
                        groups.add(frozenset(group))

            for double_mutexes in self._divide_disjoint(list(groups)):
                ps = possibles - set().union(*double_mutexes)
                if n - 2 * len(double_mutexes) == 0:
                    for possible in ps:
                        self.log(
                            f"TACTIC I:{pos} {ps} {double_mutexes}: Mining {possible}"
                        )
                        return False, possible

    def tactic_J(self):
        for pos, n in self.numeric.items():
            possibles = self.open_neighbours[pos]
            n -= self.no_flag_neighbours[pos]
            groups: set[frozenset[Pos]] = set()
            for possible in possibles:
                for group in self.double_mutexes[possible]:
                    if group.issubset(possibles):
                        groups.add(frozenset(group))

            added = False
            for double_mutexes in self._divide_disjoint(list(groups)):
                ps = possibles - set().union(*double_mutexes)
                if n - len(double_mutexes) == 2:
                    if ps not in self.double_mutex_groups:
                        added = True
                        for p in ps:
                            self.double_mutexes[p].append(ps)
                        self.double_mutex_groups.append(ps)
                        self.log(f"TACTIC J:{pos} {double_mutexes}: {ps}")
            if added:
                return self.run_tactics()

    def tactic_K(self):
        count = 0
        for v in self.game.board.values():
            count += v == "F"
        if count == self.flags:
            for pos, v in self.game.board.items():
                if v == "?":
                    self.log(f"TACTIC K:All flagged: Mining {pos}")
                    return False, pos

    def _divide_disjoint(self, lst: list[frozenset]):
        if not lst:
            yield []
        else:
            x = lst.pop()
            yield from map(
                lambda arr: arr + [x],
                self._divide_disjoint([s for s in lst if x.isdisjoint(s)]),
            )
            yield from self._divide_disjoint(lst)

    def _divide(self, lst: list):
        if not lst:
            yield []
        else:
            x = lst.pop()
            yield from map(lambda arr: arr + [x], self._divide(lst[:]))
            yield from self._divide(lst)

    def _neighbours(self, pos: Pos):
        x, y = pos
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                if dx == dy == 0:
                    continue
                xx = x + dx
                yy = y + dy
                if 0 <= xx < self.game.cols and 0 <= yy < self.game.rows:
                    yield (xx, yy)
