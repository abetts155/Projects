import atexit
import os
import subprocess

from ..graphs import edge
from ..graphs import graph
from ..utils import messages


def clean_up(program):
    for root, dirs, files in os.walk(program.basename()):
        for filename in files:
            filename = os.path.join(root, filename)
            os.remove(filename)


def visualise_call_graph(program):
    data = []
    write_call_graph(data, program.call_graph)
    filename = program.basename() + 'call.dot'
    generate(filename, data)


def write_call_graph(data, g):
    for v in g.subprograms_under_analysis():
        label = []
        label.append('<<TABLE BORDER="0">')
        label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.name))
        label.append('</TABLE>>')
        data.append('{} [label={}, shape=record];\n'.format(v.id, ''.join(label)))
        for e in g.successors(v):
            data.append('{}->{} [label="{}"];\n'.format(v.id,
                                                        e.successor().id,
                                                        ','.join(repr(v.id) for v in e.call_sites)))


def visualise_control_flow_graph(program, cfg):
    data = []
    write_control_flow_graph(data, cfg)
    filename = program.basename() + cfg.name + '.cfg.dot'
    generate(filename, data)


def write_control_flow_graph(data, g):
    def write_vertex(v):
        label = []
        label.append('<<TABLE BORDER="0">')
        label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        label.append('</TABLE>>')

        data.append('{} [label={}, shape=box];\n'.format(v.id, ''.join(label)))
        for e in g.successors(v):
            data.append('{}->{};\n'.format(v.id, e.successor().id))

    dfs = graph.DepthFirstSearch(g, g.entry)
    for v in reversed(dfs.post_order()):
        write_vertex(v)


def visualise_instrumentation_point_graph(program, ipg, suffix=''):
    data = []
    write_instrumentation_point_graph(data, ipg)
    filename = program.basename() + ipg.name + suffix + '.ipg.dot'
    generate(filename, data)


def write_instrumentation_point_graph(data, ipg):
    for v in ipg.vertices:
        label = ['<<TABLE BORDER="0">']
        if v.program_point is not None:
            if isinstance(v.program_point, edge.Edge):
                label.append('<TR><TD ALIGN="LEFT">({},{})</TD></TR>'.format(v.program_point.predecessor().id, v.program_point.successor().id))
            else:
                label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        else:
            label.append('<TR><TD></TD></TR>')
        label.append('</TABLE>>')
        data.append('{} [tooltip={}, label={}, shape=box];\n'.format(v.id, v.id, ''.join(label)))
        for e in ipg.successors(v):
            edge_label = ['<<TABLE BORDER="0">']
            for p in e.path:
                edge_label.append('<TR><TD>{}</TD></TR>'.format(str(p.program_point)))
            if not e.path:
                edge_label.append('<TR><TD></TD></TR>')
            edge_label.append('</TABLE>>')
            data.append('{}->{} [label={}];\n'.format(v.id, e.successor().id, ''.join(edge_label)))


def visualise_flow_graph(program, flow_g, suffix):
    data = []
    write_flow_graph(data, flow_g)
    filename = program.basename() + flow_g.name + suffix + '.dot'
    generate(filename, data)


def write_flow_graph(data, flow_g):
    def write_vertex(v):
        label = ['<<TABLE BORDER="0">']
        if v.program_point is not None:
            if isinstance(v.program_point, edge.Edge):
                label.append('<TR><TD ALIGN="LEFT">({},{})</TD></TR>'.format(v.program_point.predecessor().id, v.program_point.successor().id))
            else:
                label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        else:
            label.append('<TR><TD></TD></TR>')
        label.append('</TABLE>>')
        data.append('{} [label={}, shape=record];\n'.format(v.id, ''.join(label)))
        for e in flow_g.successors(v):
            data.append('{}->{};\n'.format(v.id, e.successor().id))

    for v in flow_g.vertices:
        write_vertex(v)


def visualise_dominator_tree(program, cfg, t, suffix):
    data = []
    write_dominator_tree(data, t)
    filename = program.basename() + cfg.name + suffix + '.dot'
    generate(filename, data)


def write_dominator_tree(data, t):
    for v in t.vertices:
        label = ['<<TABLE BORDER="0">']
        label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        label.append('</TABLE>>')
        data.append('{} [label={}, shape=record];\n'.format(v.id, ''.join(label)))
        for e in t.successors(v):
            data.append('{}->{};\n'.format(v.id, e.successor().id))


def visualise_loop_nest_tree(program, cfg, t):
    data = []
    write_loop_nest_tree(data, t)
    filename = program.basename() + cfg.name + '.lnt.dot'
    generate(filename, data)


def write_loop_nest_tree(data, t):
    def write_vertex(loop):
        label = []
        label.append('<<TABLE BORDER="0">')
        label.append('<TR><TD ALIGN="LEFT" BGCOLOR="RED">ID={}</TD></TR>'.format(loop.id))
        for v in loop.vertices:
            label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.program_point))
        label.append('</TABLE>>')
        data.append('{} [label={}, shape=record];\n'.format(loop.id, ''.join(label)))
        for e in t.successors(loop):
            data.append('{}->{};\n'.format(loop.id, e.successor().id))

    dfs = graph.DepthFirstSearch(t, t.root)
    for v in t.vertices:
        write_vertex(v)


child_processes = []
def generate(dot_filename, data):
    if __debug__:
        def launch_dot(ext):
            filename = os.path.splitext(dot_filename)[0] + '.' + ext
            messages.debug_message("Generating file '{}'".format(filename))
            try:
                with open(filename, 'w') as out_file:
                    cmd = ["dot", "-T", ext, dot_filename]
                    p = subprocess.Popen(cmd, stdout=out_file)
                    child_processes.append(p)
                    _, _ = p.communicate()
                    if p.returncode != 0:
                        messages.error_message("Running '{}' failed".format(' '.join(cmd)))
                    else:
                        messages.debug_message("Done with '{}'".format(' '.join(cmd)))
            except FileNotFoundError as e:
                messages.debug_message(e)

        with open(dot_filename, 'w') as dot_file:
            dot_file.write('digraph')
            dot_file.write('{\n')
            dot_file.write('nslimit=2;\n')
            dot_file.write('ordering=out;\n')
            dot_file.write('ranksep=0.3;\n')
            dot_file.write('nodesep=0.25;\n')
            dot_file.write('fontsize=8;\n')
            dot_file.write('fontname="Times new roman"\n')
            for d in data:
                dot_file.write(d)
            dot_file.write('}\n')

        launch_dot('png')
        os.remove(dot_filename)


def kill_child_processes():
    for p in child_processes:
        p.kill()


atexit.register(kill_child_processes)
