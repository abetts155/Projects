import dataclasses
import enum
import math


@dataclasses.dataclass(frozen=True, slots=True)
class Coordinate:
    x: int
    y: int

    def __post_init__(self):
        assert self.x >= 0 and self.y >= 0

    def __str__(self):
        return f'({self.x, self.y})'


@dataclasses.dataclass(frozen=True, slots=True)
class Line:
    a: Coordinate
    b: Coordinate

    def __post_init__(self):
        assert self.a.x == self.b.x or self.a.y == self.b.y

    @property
    def length(self):
        if self.a.x == self.b.x:
            return abs(self.a.y - self.b.y)
        else:
            return abs(self.a.x - self.b.x)


@dataclasses.dataclass(frozen=True, slots=True)
class Rectangle:
    """The lines of the rectangle must satisfy this layout:
    C1 ---- C2
    |       |
    C3 ---- C4
    """

    C1: Coordinate
    C2: Coordinate
    C3: Coordinate
    C4: Coordinate

    def __post_init__(self):
        print(self.C1, self.C2, self.C3, self.C4)
        assert self.C1.y == self.C2.y
        assert self.C1.x == self.C3.x
        assert self.C2.x == self.C4.x
        assert self.C3.y == self.C4.y
        assert self.C2.x > self.C1.x
        assert self.C4.x > self.C3.x
        assert self.C3.y > self.C1.y

    @property
    def width(self):
        return self.C2.x - self.C1.x

    @property
    def height(self):
        return self.C3.y - self.C1.y


class PitchSide(enum.Enum):
    LEFT = enum.auto()
    RIGHT = enum.auto()


class Post(enum.Enum):
    NEAR = enum.auto()
    FAR = enum.auto()


def create_internal_rectangle(side: PitchSide, reference: Rectangle, x_adjustment: int, y_adjustment: int) -> Rectangle:
    if side == PitchSide.LEFT:
        return Rectangle(Coordinate(reference.C1.x, reference.C1.y + y_adjustment),
                         Coordinate(reference.C1.x + x_adjustment, reference.C1.y + y_adjustment),
                         Coordinate(reference.C3.x, reference.C3.y - y_adjustment),
                         Coordinate(reference.C3.x + x_adjustment, reference.C3.y - y_adjustment))
    else:
        return Rectangle(Coordinate(reference.C2.x - x_adjustment, reference.C2.y + y_adjustment),
                         Coordinate(reference.C2.x, reference.C2.y + y_adjustment),
                         Coordinate(reference.C4.x - x_adjustment, reference.C4.y - y_adjustment),
                         Coordinate(reference.C4.x, reference.C4.y - y_adjustment))


class Pitch:
    def __init__(self, width: int = 116, height: int = 74):
        c1 = Coordinate(0, 0)
        c2 = Coordinate(width, 0)
        c3 = Coordinate(0, height)
        c4 = Coordinate(width, height)
        self.field = Rectangle(c1, c2, c3, c4)

    @property
    def centre_spot(self) -> Coordinate:
        return Coordinate(self.field.width // 2, self.field.height // 2)

    @property
    def circle_area(self) -> float:
        circle_radius = 10
        return 2 * circle_radius * math.pi

    def penalty_spot_position(self, side: PitchSide) -> Coordinate:
        adjustment = 12
        if side == PitchSide.LEFT:
            return Coordinate(self.field.C1.x + adjustment, self.field.width // 2)
        else:
            return Coordinate(self.field.C1.x - adjustment, self.field.width // 2)

    def post_position(self, side: PitchSide, post: Post) -> Coordinate:
        adjustment = 33
        if side == PitchSide.LEFT:
            if post == Post.FAR:
                return Coordinate(self.field.C1.x, self.field.C1.y + adjustment)
            else:
                return Coordinate(self.field.C3.x, self.field.C3.y - adjustment)
        else:
            if post == Post.FAR:
                return Coordinate(self.field.C2.x, self.field.C2.y + adjustment)
            else:
                return Coordinate(self.field.C4.x, self.field.C4.y - adjustment)

    def penalty_area(self, side: PitchSide) -> Rectangle:
        x_adjustment = 18
        y_adjustment = 15
        return create_internal_rectangle(side, self.field, x_adjustment, y_adjustment)

    def six_yard_area(self, side: PitchSide) -> Rectangle:
        x_adjustment = 6
        y_adjustment = 27
        return create_internal_rectangle(side, self.field, x_adjustment, y_adjustment)
