import enum
import typing

from ast import ast


RVS_DIRECTIVE = "RVS"


class Analysis(enum.Enum):
    TIMING = 0
    COVERAGE = 1


class Annotation(enum.Enum):
    INSTRUMENT = 0
    DEFAULT_INSTRUMENT = 1


class InstrumentationProfile(enum.Enum):
    DEFAULT = 0
    MANUAL = 1
    TIME_FULL = 2
    TIME_FUNCTIONS = 3
    TIME_START_OF_SCOPES = 4
    COV_178_DAL_A = 5
    COV_178_DAL_B = 6
    COV_178_DAL_C = 7
    COV_MCDC = 8
    COV_STATEMENTS = 9
    COV_CALLS = 10
    COV_BRANCHES = 11
    COV_DECISIONS = 12
    COV_FUNCTIONS = 13
    COV_FUNCTION_EXITS = 14
    COV_26262_HR_ASIL_A = 15
    COV_26262_HR_ASIL_B = 16
    COV_26262_HR_ASIL_C = 17

    @classmethod
    def timing_profiles(cls):
        return [cls.DEFAULT,
                cls.MANUAL,
                cls.TIME_FULL,
                cls.TIME_FUNCTIONS,
                cls.TIME_START_OF_SCOPES]

    @classmethod
    def coverage_profiles(cls):
        return [cls.DEFAULT,
                cls.MANUAL,
                cls.COV_178_DAL_A,
                cls.COV_178_DAL_B,
                cls.COV_178_DAL_C,
                cls.COV_MCDC,
                cls.COV_STATEMENTS,
                cls.COV_CALLS,
                cls.COV_BRANCHES,
                cls.COV_DECISIONS,
                cls.COV_FUNCTIONS,
                cls.COV_FUNCTION_EXITS,
                cls.COV_26262_HR_ASIL_A,
                cls.COV_26262_HR_ASIL_B,
                cls.COV_26262_HR_ASIL_C]


class InstrumentAnnotation:
    def __init__(self,
                 language: ast.Language,
                 profile: InstrumentationProfile,
                 subprogram_declaration: ast.SubprogramDecl,
                 on: bool = True):
        self._language = language
        self._profile = profile
        self._subprogram_declaration = subprogram_declaration
        self._on = on

    def unparse(self, stream: typing.TextIO):
        stream.write(ast.Punctuation.HASH.value)
        stream.write(ast.Keywords.PRAGMA.name.lower())
        stream.write(ast.Punctuation.SPACE.value)
        stream.write(RVS_DIRECTIVE)
        stream.write(ast.Punctuation.SPACE.value)

        if not self._subprogram_declaration.name:
            stream.write(Annotation.DEFAULT_INSTRUMENT.name.lower())
            stream.write(ast.Punctuation.OPEN_PARENTHESIS.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(str(True))
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.COMMA.value)
            stream.write(ast.Punctuation.SPACE.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(self._profile.name)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.CLOSE_PARENTHESIS.value)
            stream.write(ast.Punctuation.SEMI_COLON.value)
        elif self._profile != InstrumentationProfile.DEFAULT:
            stream.write(Annotation.INSTRUMENT.name.lower())
            stream.write(ast.Punctuation.OPEN_PARENTHESIS.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            self._subprogram_declaration.name.unparse(stream)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.COMMA.value)
            stream.write(ast.Punctuation.SPACE.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(str(self._on))
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.COMMA.value)
            stream.write(ast.Punctuation.SPACE.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(self._profile.name)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.CLOSE_PARENTHESIS.value)
            stream.write(ast.Punctuation.SEMI_COLON.value)
        else:
            stream.write(Annotation.INSTRUMENT.name.lower())
            stream.write(ast.Punctuation.OPEN_PARENTHESIS.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            self._subprogram_declaration.name.unparse()
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.COMMA.value)
            stream.write(ast.Punctuation.SPACE.value)
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(str(self._on))
            stream.write(ast.Punctuation.DOUBLE_QUOTES.value)
            stream.write(ast.Punctuation.CLOSE_PARENTHESIS.value)
            stream.write(ast.Punctuation.SEMI_COLON.value)
