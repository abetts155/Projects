import atexit
import os
import subprocess

from ..utils import messages
from ..graphs import graph


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
    os.remove(filename)


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
    os.remove(filename)


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
    os.remove(filename)


def write_instrumentation_point_graph(data, ipg):
    for v in ipg.vertices:
        label = []
        label.append('<<TABLE BORDER="0">')
        label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.inlined if v.inlined else ''))
        label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        for i in v.instructions:
            label.append('<TR>')
            label.append('<TD ALIGN="LEFT" BORDER="2">{}</TD>'.format(i.id))
            label.append('<TD ALIGN="LEFT">{}</TD>'.format(hex(i.address)))
            label.append('<TD ALIGN="LEFT">{}</TD>'.format(i.opcode))
            for o in i.operands:
                label.append('<TD ALIGN="LEFT">{}</TD>'.format(o))
            label.append('</TR>')
        label.append('</TABLE>>')

        data.append('{} [tooltip={}, label={}, shape=box];\n'.format(v.id, v.id, ''.join(label)))

        bg_color = 'cyan'
        for e in ipg.successors(v):
            edge_label = []
            edge_label.append('<<TABLE BORDER="0">')
            for i in e.instructions:
                edge_label.append('<TR>')
                edge_label.append('<TD ALIGN="LEFT" BORDER="2">{}</TD>'.format(i.id))
                edge_label.append('<TD ALIGN="LEFT" BGCOLOR="{}">{}</TD>'.format(bg_color, hex(i.address)))
                edge_label.append('<TD ALIGN="LEFT" BGCOLOR="{}">{}</TD>'.format(bg_color, i.opcode))
                for o in i.operands:
                    edge_label.append('<TD ALIGN="LEFT" BGCOLOR="{}">{}</TD>'.format(bg_color, o))
                edge_label.append('</TR>')
            if not e.instructions:
                edge_label.append('<TR><TD></TD></TR>')
            edge_label.append('</TABLE>>')
            data.append('{}->{} [label={}];\n'.format(v.id, e.successor().id, ''.join(edge_label)))


def visualise_flow_graph(program, flow_g, suffix):
    data = []
    write_flow_graph(data, flow_g)
    filename = program.basename() + flow_g.name + suffix + '.dot'
    generate(filename, data)
    os.remove(filename)


def write_flow_graph(data, flow_g):
    def write_vertex(v):
        label = []
        label.append('<<TABLE BORDER="0">')
        if isinstance(v, vertex.ProgramPoint):
            label.append('<TR><TD ALIGN="LEFT">({},{})</TD></TR>'.format(v.edge.predecessor().id, v.edge.successor().id))
        else:
            label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        label.append('</TABLE>>')
        data.append('{} [label={}, shape=record];\n'.format(v.id, ''.join(label)))
        for e in flow_g.successors(v):
            data.append('{}->{};\n'.format(v.id, e.successor().id))

    write_vertex(flow_g.entry)
    for v in (_ for _ in flow_g.vertices if _ != flow_g.entry):
        write_vertex(v)


def visualise_dominator_tree(program, cfg, t, suffix):
    data = []
    write_dominator_tree(data, t)
    filename = program.basename() + cfg.name + suffix + '.dot'
    generate(filename, data)
    os.remove(filename)


def write_dominator_tree(data, t):
    for v in t.vertices:
        label = []
        label.append('<<TABLE BORDER="0">')
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
    os.remove(filename)


def write_loop_nest_tree(data, t):
    def write_vertex(loop):
        label = []
        label.append('<<TABLE BORDER="0">')
        label.append('<TR><TD ALIGN="LEFT" BGCOLOR="RED">ID={}</TD></TR>'.format(loop.id))
        for v in loop.vertices:
            if isinstance(v, vertex.ProgramPoint):
                label.append('<TR><TD ALIGN="LEFT">({}, {})</TD></TR>'.format(v.edge.predecessor().id, v.edge.successor().id))
            else:
                label.append('<TR><TD ALIGN="LEFT">{}</TD></TR>'.format(v.id))
        label.append('</TABLE>>')
        data.append('{} [label={}, shape=record];\n'.format(loop.id, ''.join(label)))
        for e in t.successors(loop):
            data.append('{}->{};\n'.format(loop.id, e.successor().id))

    dfs = graph.DepthFirstSearch(t, t.root)
    for v in reversed(dfs.post_order()):
        write_vertex(v)


child_processes = []
def generate(dot_filename, data):
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

    if __debug__:
        launch_dot('png')
    else:
        launch_dot('svg')


def kill_child_processes():
    for p in child_processes:
        p.kill()


atexit.register(kill_child_processes)
