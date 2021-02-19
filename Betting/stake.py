class Bet:
    __slots__ = ['spent', 'profit', 'odds', 'commission']

    def __init__(self, spent, profit, odds, commission):
        self.spent = spent
        self.profit = profit
        self.odds = odds
        self.commission = commission

    def wager(self) -> float:
        return (self.spent + self.profit) / (self.commission * (self.odds - 1))


def extract(value: str) -> Bet:
    try:
        lexemes = value.split()
        if len(lexemes) == 3:
            a, b, c = lexemes
            return Bet(int(a), int(b), float(c), 1)
        else:
            a, b, c, d = lexemes
            return Bet(int(a), int(b), float(c), float(d))
    except ValueError:
        print("Invalid input '{}'".format(value))


def main():
    while True:
        try:
            arguments = input('>>> In a space-separated list input '
                              '(a) spent '
                              '(b) profit '
                              '(c) odds and '
                              '(d) commission (optional):\n')
            bet = extract(arguments.strip())
            if bet is not None:
                total_spend = bet.spent + bet.wager()
                total_return = bet.odds * bet.wager()
                print('Placing a bet of £{:.2f} returns £{:.2f}, which is a profit of £{:.2f}, '
                      'having spent £{:.2f} in total.'.format(bet.wager(), total_return, bet.profit, total_spend))
        except KeyboardInterrupt:
            print('\nExiting')
            exit(0)


if __name__ == '__main__':
    main()
