from datasets import load_dataset
import re
import yaml
from datetime import datetime
import json 


# Load dataset
print("Loading dataset from hugging face . . .")
dataset = load_dataset("Anthropic/hh-rlhf")

for i in range(0,15):
    text = dataset["train"]["chosen"][i]
    content = re.split(r'\n\n(?:Human|Assistant):\s', text)[1:]
    dialogues = []
    for i in range(0, len(content), 2):
        user_dialog = {"role": "user", "content": content[i]}
        assistant_dialog = {"role": "assistant", "content": content[i + 1]}
        dialogues.extend([user_dialog, assistant_dialog])

"""
current_datetime = datetime.now().strftime("%Y_%m_%d-%H_%M_%S")
filename = "fine_tuning/fine_tuning_" + current_datetime + ".yaml"

with open(filename, "w+") as outfile: 
    for msg in dialogues:
        json.dump(msg, outfile)
        outfile.write('\n')

"""










