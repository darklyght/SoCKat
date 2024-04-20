import argparse
import jinja2
import os
import re
import subprocess

script_path = os.path.dirname(os.path.realpath(__file__))

densities = {
    "den1024Mb": f"{script_path}/../../../../../../sim/lib/DDR3_SDRAM_Verilog_Model/1024Mb_ddr3_parameters.vh",
    "den2048Mb": f"{script_path}/../../../../../../sim/lib/DDR3_SDRAM_Verilog_Model/2048Mb_ddr3_parameters.vh",
    "den4096Mb": f"{script_path}/../../../../../../sim/lib/DDR3_SDRAM_Verilog_Model/4096Mb_ddr3_parameters.vh",
    "den8192Mb": f"{script_path}/../../../../../../sim/lib/DDR3_SDRAM_Verilog_Model/8192Mb_ddr3_parameters.vh"
}

speed_grades = [
    "sg15",
    "sg15E",
    "sg25",
    "sg25E",
    "sg093",
    "sg107",
    "sg125",
    "sg187",
    "sg187E"
]

widths = [
    "x16",
    "x8",
    "x4"
]

ranks = [
    "QUAD_RANK",
    "DUAL_RANK",
    "SINGLE_RANK"
]

values = {}

for density in densities:
    parameters = {}

    with open (densities[density], "r") as f:
        contents = f.read()
        lines = re.findall("parameter\s+([A-Z0-9_]+)\s+.*//\s+([ \w()]+)", contents)
        for line in lines:
            unit = "Int"
            if " tCK " in line[1]:
                unit = "tCK"
            if " ps " in line[1]:
                unit = "ps"
        
            parameters[line[0]] = unit

    environment = jinja2.Environment(loader = jinja2.FileSystemLoader(f"{script_path}"))
    template = environment.get_template("ddr3_parameter_generation.template")
    content = template.render(parameters = parameters)

    with open(f"{script_path}/ddr3_parameter_generation.v", "w+") as f:
        f.write(content)

    output = []

    for speed_grade in speed_grades:
        for width in widths:
            for rank in ranks:
                process = subprocess.Popen(f"iverilog -I {script_path}/../../../../../../sim/lib/DDR3_SDRAM_Verilog_Model -D {density} -D {speed_grade} -D {width} -D{rank} {script_path}/ddr3_parameter_generation.v -o {script_path}/ddr3_parameter_generation && vvp {script_path}/ddr3_parameter_generation",
                                        stdout = subprocess.PIPE,
                                        stderr = subprocess.PIPE,
                                        bufsize = 1,
                                        universal_newlines = True,
                                        shell = True,
                                        executable = "/usr/bin/bash")
                
                for line in iter(process.stdout.readline, ""):
                    output.append(line.rstrip("\n"))

                for line in output:
                    content = re.search("([A-Z0-9_]+)\s*([0-9\.]+)", line)
                    parameter = content[1]

                    if parameter not in parameters:
                        print(line)
                        raise KeyError(f"Parameter {parameter} not found.")
                    
                    value = int(content[2]) if parameters[parameter] == "Int" else float(content[2])
                    
                    if parameter not in values:
                        values[parameter] = {}
                        values[parameter]["value"] = {}
                    
                    values[parameter]["value"][(density, speed_grade, width, rank)] = value
                    values[parameter]["unit"] = parameters[parameter]

parser = argparse.ArgumentParser()
parser.add_argument("density")
parser.add_argument("speed_grade")
parser.add_argument("width")
parser.add_argument("rank")
args = parser.parse_args()

if args.density not in densities:
    raise ValueError("Invalid density.")

if args.speed_grade not in speed_grades:
    raise ValueError("Invalid speed grade.")

if args.width not in widths:
    raise ValueError("Invalid width.")

if args.rank not in ranks:
    raise ValueError("Invalid rank.")

result = {}

for parameter in values:
    if parameter not in result:
        result[parameter] = {}
        result[parameter]["value"] = {}

    result[parameter]["value"][(args.density, args.speed_grade, args.width, args.rank)] = values[parameter]["value"][(args.density, args.speed_grade, args.width, args.rank)]
    result[parameter]["unit"] = values[parameter]["unit"]

template = environment.get_template("device_parameters.template")
content = template.render(
    density = args.density,
    speed_grade = args.speed_grade,
    width = args.width,
    rank = args.rank,
    parameters = result
)

with open(f"{script_path}/../device_parameters.scala", "w+") as f:
    f.write(content.replace("'", '"'))