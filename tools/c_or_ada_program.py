import sys

import multiprocessing
import random
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
        annotation = annotations.InstrumentAnnotation(language, default_profile, subprogram)
        subprogram.add_annotation(annotation)
    else:
        if can_override_default and helpful.go_ahead(0.1):
            if analysis == annotations.Analysis.TIMING:
                profile = random.choice(annotations.InstrumentationProfile.timing_profiles())
            else:
                profile = random.choice(annotations.InstrumentationProfile.coverage_profiles())
            annotation = annotations.InstrumentAnnotation(language,
                                                          profile,
                                                          subprogram,
                                                          helpful.go_ahead())
            subprogram.add_annotation(annotation)

    for nested_subprogram in subprogram.subprograms:
        create_annotations(language, analysis, default_profile, can_override_default, nested_subprogram)


def create_subprogram(subprogram:        ast.Subprogram,
                      expression_depth:  int,
                      ignore_operators:  typing.List[ast.ArithmeticOperator],
                      block_length:      int,
                      basic_block_limit: int,
                      number_of_loops:   int,
                      nesting_depth:     int,
                      call_graph:        graph.DirectedGraph):
    helpful.debug_message("Creating subprogram definition '{}'".format(subprogram.subprogram_declaration.name))
    loop_hierarchy = graph.random_loop_hierarchy(basic_block_limit, number_of_loops, nesting_depth)
    subprogram.cfg = graph.random_control_flow_graph(loop_hierarchy)
    subprogram.generate_body(expression_depth,
                             ignore_operators,
                             block_length,
                             call_graph)
    return subprogram


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
    subprograms = []
    for subprogram_declaration in subprogram_declarations:
        subprogram = ast.Subprogram(language, subprogram_declaration)
        subprogram.main = subprogram_declaration == main_declaration
        subprograms.append(subprogram)

    parallel_data = [(subprogram,
                      expression_depth,
                      ignore_operators,
                      block_length,
                      basic_block_limit,
                      number_of_loops,
                      nesting_depth,
                      call_graph) for subprogram in subprograms]

    max_workers = 8
    subprograms = []
    with multiprocessing.Pool(max_workers) as executor:
        subprograms.extend(executor.starmap(create_subprogram, parallel_data))

    if language == ast.Language.C:
        top_level_subprogram = ast.Subprogram(language, ast.SubprogramDecl(language, None))
    else:
        (top_level_subprogram,) = [subprogram for subprogram in subprograms if subprogram.main]

    for subprogram in subprograms:
        if language == ast.Language.C or (language == ast.Language.ADA and not subprogram.main):
            top_level_subprogram.add_subprogram(subprogram)

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

    helpful.verbose_message('Generating subprogram declarations')
    main_declaration, subprogram_declarations = create_subprogram_declarations(args['language'],
                                                                               args['subprograms'],
                                                                               args['formal_parameter_limit'])

    helpful.verbose_message('Generating call graph')
    call_graph = graph.random_call_graph(main_declaration,
                                         subprogram_declarations,
                                         args['call_depth'],
                                         args['allow_recursion'])

    helpful.verbose_message('Generating subprogram definitions')
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

    helpful.verbose_message('Generating annotations')
    create_annotations(args['language'],
                       args['analysis'],
                       args['default_instrumentation'],
                       not args['no_instrumentation_override'],
                       top_level_subprogram)

    helpful.verbose_message('Writing to output')
    if args['filename']:
        with open(args['filename'], 'w') as stream:
            top_level_subprogram.unparse(stream)
    else:
        top_level_subprogram.unparse(sys.stdout)
