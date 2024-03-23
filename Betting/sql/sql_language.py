from enum import Enum, auto


class Characters(Enum):
    QUESTION_MARK = '?'
    STAR = '*'
    FALSE = '0'
    TRUE = '1'


class Keywords(Enum):
    AND = auto()
    COLLATE = auto()
    COUNT = auto()
    CREATE = auto()
    DROP = auto()
    EXISTS = auto()
    FOREIGN = auto()
    FROM = auto()
    IF = auto()
    IN = auto()
    INSERT = auto()
    INTO = auto()
    KEY = auto()
    LIKE = auto()
    NOCASE = auto()
    NOT = auto()
    NULL = auto()
    OR = auto()
    PRIMARY = auto()
    REFERENCES = auto()
    REPLACE = auto()
    SELECT = auto()
    TABLE = auto()
    VALUES = auto()
    WHERE = auto()
