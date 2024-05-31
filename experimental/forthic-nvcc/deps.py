import subprocess

def cmd(string):
    try:
        result = subprocess.check_output([string], shell=True).decode('utf-8')[:-1]
    except:
        result = ""
    return result


def dot_h_files():
    return cmd("ls *.h m_*/*.h test/*.h").split("\n")


def header_dependencies(header):

    def grep_header(extension):
        def filename(f):
            return f.split("/")[-1]
        return cmd("grep {0} *{1} m_*/*{1} examples/*{1}  test/*{1}".format(filename(header), extension)).split("\n")

    def dot_o(line, extension):
        base = line.split(extension)[0]
        if not base:
            return ""
        return base + ".o"

    def deps(extension):
        lines = grep_header(extension)
        return [dot_o(l, extension) for l in lines]

    dot_os = deps(".cu") + deps(".cpp") + deps(".h")

    result = ""
    if dot_os:
        result = "{0} : {1}".format(" ".join(dot_os), header)
    return result


def dependencies():
    return "\n".join([ header_dependencies(h) for h in dot_h_files()])

print(dependencies())
