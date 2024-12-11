import os
import subprocess

# Define the directories
corpus_dir = './rtlil-corpus/corpus/'
parsed_dir = './parsed_rtlil'

# Create the parsed_rtlil directory if it doesn't exist
os.makedirs(parsed_dir, exist_ok=True)

# Iterate over all .rtlil files in the corpus directory
for file_name in os.listdir(corpus_dir):
    if file_name.endswith('.il'):
        input_path = os.path.join(corpus_dir, file_name)
        output_file = os.path.splitext(file_name)[0] + '.hs'
        output_path = os.path.join(parsed_dir, output_file)

        # Construct and run the command
        command = f"cabal run rtlil-parse -- {input_path} {output_path}"
        try:
            subprocess.run(command, shell=True, check=True)
            print(f"Processed: {input_path} -> {output_path}")
        except subprocess.CalledProcessError as e:
            print(f"Error processing {input_path}: {e}")
            break
