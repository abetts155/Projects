import random
import sys
import typing

from ast import (annotations, ast)
from graphs import graph
from miscellaneous import (command_line, helpful)


def create_annotations(language:             ast.Language,
                       analysis:             annotations.Analysis,
                       default_profile:      annotations.InstrumentationProfile,
                       can_override_default: bool,
                       subprogram:           ast.Subprogram):
    if subprogram.fake or (subprogram.main and language == ast.Language.ADA):
        annotation = annotations.InstrumentAnnotation(language, default_profile, subprogram.subprogram_declaration)
        subprogram.add_annotation(annotation)
    else:
        if can_override_default and helpful.go_ahead(0.1):
            if analysis == annotations.Analysis.TIMING:
                profile = random.choice(annotations.InstrumentationProfile.timing_profiles())
            else:
                profile = random.choice(annotations.InstrumentationProfile.coverage_profiles())
            annotation = annotations.InstrumentAnnotation(language,
                                                          profile,
                                                          subprogram.subprogram_declaration,
                                                          helpful.go_ahead())
            subprogram.add_annotation(annotation)

    for nested_subprogram in subprogram.subprograms:
        create_annotations(language, analysis, default_profile, can_override_default, nested_subprogram)


def create_subprogram_definitions(language:                ast.Language,
                                  expression_depth:        int,
                                  ignore_operators:        typing.List[ast.ArithmeticOperator],
                                  block_length:            int,
                                  basic_block_limit:       int,
                                  number_of_loops:         int,
                                  nesting_depth:           int,
                                  subprogram_declarations: typing.List[ast.SubprogramDecl],
                                  main_declaration:        ast.SubprogramDecl,
                                  call_graph:              graph.DirectedGraph) -> ast.Subprogram:

    if language == ast.Language.C:
        top_level_subprogram = ast.Subprogram(language, ast.SubprogramDecl(language, None))
    else:
        top_level_subprogram = ast.Subprogram(language, main_declaration)

    for subprogram_declaration in subprogram_declarations:
        if subprogram_declaration == main_declaration:
            if language == ast.Language.C:
                subprogram = ast.Subprogram(language, subprogram_declaration)
                subprogram.main = True
                top_level_subprogram.add_subprogram(subprogram)
            else:
                subprogram = top_level_subprogram
        else:
            subprogram = ast.Subprogram(language, subprogram_declaration)
            top_level_subprogram.add_subprogram(subprogram)

        loop_hierarchy = graph.random_loop_hierarchy(basic_block_limit, number_of_loops, nesting_depth)
        subprogram.cfg = graph.random_control_flow_graph(loop_hierarchy)
        subprogram.generate_body(expression_depth,
                                 ignore_operators,
                                 block_length,
                                 call_graph)

    return top_level_subprogram


def create_subprogram_declarations(language:               ast.Language,
                                   number_of_subprograms:  int,
                                   formal_parameter_limit: int) -> typing.Tuple[ast.SubprogramDecl,
                                                                                typing.List[ast.SubprogramDecl]]:
    subprogram_declarations = []
    main_declaration = None
    for subprogram_id in range(1, number_of_subprograms+1):
        if subprogram_id == number_of_subprograms:
            main_declaration = ast.SubprogramDecl(language, ast.Identifier('main'))
            if language == ast.Language.C:
                main_declaration.return_type = ast.IntegerType(language)
            else:
                main_declaration.return_type = ast.VoidType()
            subprogram_declarations.append(main_declaration)
        else:
            if helpful.go_ahead(0.8):
                declaration = ast.SubprogramDecl(language, ast.Identifier('func_{}'.format(subprogram_id)))
                declaration.return_type = ast.IntegerType(language)
            else:
                declaration = ast.SubprogramDecl(language, ast.Identifier('proc_{}'.format(subprogram_id)))
                declaration.return_type = ast.VoidType()
            declaration.create_formals(random.randint(1, formal_parameter_limit))
            subprogram_declarations.append(declaration)
    return main_declaration, subprogram_declarations


if __name__ == '__main__':
    args = command_line.parse()
    command_line.standardise_arguments(args)
    command_line.check_arguments(args)

    main_declaration, subprogram_declarations = create_subprogram_declarations(args['language'],
                                                                               args['subprograms'],
                                                                               args['formal_parameter_limit'])

    call_graph = graph.random_call_graph(main_declaration,
                                         subprogram_declarations,
                                         args['call_depth'],
                                         args['allow_recursion'])

    top_level_subprogram = create_subprogram_definitions(args['language'],
                                                         args['expression_depth'],
                                                         args['ignore_operators'],
                                                         args['block_length'],
                                                         args['basic_blocks'],
                                                         args['loops'],
                                                         args['loop_depth'],
                                                         subprogram_declarations,
                                                         main_declaration,
                                                         call_graph)

    create_annotations(args['language'],
                       args['analysis'],
                       args['default_instrumentation'],
                       not args['no_instrumentation_override'],
                       top_level_subprogram)

    if args['filename']:
        with open(args['filename'], 'w') as wd:
            top_level_subprogram.unparse(wd)
    else:
        top_level_subprogram.unparse(sys.stdout)
