class Bet:
    __slots__ = ['spent', 'profit', 'odds']

    def __init__(self, spent, profit, odds):
        self.spent = spent
        self.profit = profit
        self.odds = odds

    def wager(self) -> int:
        return round((self.spent + self.profit) / (self.odds - 1))


def extract(input_string: str) -> Bet:
    try:
        x, y, z = input_string.split()
        return Bet(int(x), int(y), float(z))
    except ValueError:
        print("Invalid input '{}'".format(input_string))


def main():
    while True:
        try:
            arguments = input('>>> In a space-separated list input (a) spent (b) profit and (c) odds:\n')
            bet = extract(arguments.strip())
            if bet is not None:
                total_spend = bet.spent + bet.wager()
                total_return = round(bet.odds * bet.wager())
                print('Placing a bet of £{} returns £{}, which is a profit of £{}, '
                      'having spent £{} in total.'.format(bet.wager(), total_return, bet.profit, total_spend))
        except KeyboardInterrupt:
            print('\nExiting')
            exit(0)


if __name__ == '__main__':
    main()
