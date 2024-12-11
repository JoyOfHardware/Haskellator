import os
import subprocess
import argparse
import shutil

parser = argparse.ArgumentParser(description="Test rtlil-parse")
parser.add_argument("--corpus", default=os.environ.get('CORPUS'), required=False, help="Path to corpus dir.")
args = parser.parse_args()

# Define the directories
if args.corpus:
    corpus_dir = args.corpus
else:
    corpus_dir = './rtlil-corpus/corpus/'

parsed_dir = './parsed_rtlil'

# Create the parsed_rtlil directory if it doesn't exist
os.makedirs(parsed_dir, exist_ok=True)

# Use rtlil-parse from PATH if available.
rtil_parse_binary = shutil.which("rtlil-parse")
if rtil_parse_binary is None:
    program = "cabal run rtlil-parse --"
else:
    program = rtil_parse_binary

# Iterate over all .rtlil files in the corpus directory
for file_name in os.listdir(corpus_dir):
    if file_name.endswith('.il'):
        input_path = os.path.join(corpus_dir, file_name)
        output_file = os.path.splitext(file_name)[0] + '.hs'
        output_path = os.path.join(parsed_dir, output_file)

        # Construct and run the command
        command = f"{program} {input_path} {output_path}"
        try:
            subprocess.run(command, shell=True, check=True)
            print(f"Processed: {input_path} -> {output_path}")
        except subprocess.CalledProcessError as e:
            print(f"Error processing {input_path}: {e}")
            break
