import enum


class Characters(enum.Enum):
    QUESTION_MARK = '?'
    STAR = '*'
    FALSE = '0'
    TRUE = '1'


class Keywords(enum.Enum):
    AND = enum.auto()
    COLLATE = enum.auto()
    COUNT = enum.auto()
    CREATE = enum.auto()
    DROP = enum.auto()
    EXISTS = enum.auto()
    FOREIGN = enum.auto()
    FROM = enum.auto()
    IF = enum.auto()
    INSERT = enum.auto()
    INTO = enum.auto()
    KEY = enum.auto()
    LIKE = enum.auto()
    NOCASE = enum.auto()
    NOT = enum.auto()
    NULL = enum.auto()
    OR = enum.auto()
    PRIMARY = enum.auto()
    REFERENCES = enum.auto()
    REPLACE = enum.auto()
    SELECT = enum.auto()
    TABLE = enum.auto()
    VALUES = enum.auto()
    WHERE = enum.auto()
