class Instrumentation:
    def __init__(self, number):
        self.__number = number

    @property
    def number(self):
        return self.__number

    @number.setter
    def number(self, value):
        self.__number = value


def is_ghost(i: Instrumentation):
    return i.number == 0
