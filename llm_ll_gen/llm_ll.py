import argparse
import os

import openai
import tiktoken
import yaml
from openai import AzureOpenAI
from retry import retry

# =======================================================================#
#                            GLOBAL VALUES                               #
# =======================================================================#
LLM_VERSION  = ""
API_KEY_NAME = ""
ENDPOINT     = ""
API_VERSION  = ""
# =======================================================================#


# ===================================================================================================================#
#                                           INSERT TEST_CASE INTO A FILE                                            #
# ===================================================================================================================#
def insert_test_case_into_file(file_path, test_case):
    try:
        with open(
            file_path, "w"
        ) as file:  # DO NOT APPEND # with open(file_path, "a") as file: # APPEND #
            # Inserting the content at the beginning of the file
            file.write(test_case)

        print("Content inserted successfully.")
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
    except Exception as e:
        print(f"An error occurred: {e}")


# ===================================================================================================================#
#                                   EXTRACT A PROLOG CODE FROM LLM OUTPUT                                           #
# ===================================================================================================================#
def extract_prolog_from_llm_output(multi_line_string):
    start_delimiter = "```"
    end_delimiter = "```"
    start_pos_prlg = multi_line_string.find(start_delimiter)
    end_pos_prlg = multi_line_string.find(
        end_delimiter, start_pos_prlg + len(start_delimiter)
    )

    print(
        "First code block start: ", start_pos_prlg, ", end_pos_domain: ", end_pos_prlg
    )

    if start_pos_prlg == -1 or end_pos_prlg == -1:
        return None
    return multi_line_string[
        start_pos_prlg + len(start_delimiter) : end_pos_prlg
    ].strip()


# ===================================================================================================================#
#                                                   Send Query                                                      #
# ===================================================================================================================#
@retry(tries=2, delay=30)
def connect_openai(
    engine,
    messages,
    temperature,
    max_tokens,
    top_p,
    frequency_penalty,
    presence_penalty,
    seed,
    stop,
):
    # UNITN-key
    client = AzureOpenAI(
        api_key=os.environ[API_KEY_NAME],
        azure_endpoint=ENDPOINT,
        api_version=API_VERSION,
    )
    response = client.chat.completions.create(
        model=engine,
        messages=messages,
        temperature=temperature,
        max_tokens=max_tokens,
        top_p=top_p,
        frequency_penalty=frequency_penalty,
        presence_penalty=presence_penalty,
        seed=seed,
        stop=stop,
    )
    return response.choices[0].message.content


# ===================================================================================================================#
#                                               GPT Class                                                           #
# ===================================================================================================================#
class GPT_model:
    def __init__(
        self,
        engine,
        stop=None,
        max_tokens=2000,
        temperature=0.0,
        top_p=0,
        frequency_penalty=0.0,
        presence_penalty=0.0,
        seed=42,
    ):
        self.engine = engine
        self.max_tokens = max_tokens
        self.temperature = temperature  # 0
        self.top_p = top_p
        self.freq_penalty = frequency_penalty  # 0
        self.presence_penalty = presence_penalty  # 0
        self.stop = stop
        self.seed = seed  # 42

    def get_response(self, prompt, messages=None, end_when_error=False, max_retry=5):
        conn_success, llm_output = False, ""
        if messages is not None:
            messages.append({"role": "user", "content": prompt})
        else:
            messages = [{"role": "user", "content": prompt}]
        n_retry = 0
        while not conn_success:
            n_retry += 1
            if n_retry >= max_retry:
                break
            try:
                print("[INFO] connecting to the LLM ...")
                llm_output = connect_openai(
                    engine=self.engine,
                    messages=messages,
                    temperature=self.temperature,
                    max_tokens=self.max_tokens,
                    top_p=self.top_p,
                    frequency_penalty=self.freq_penalty,
                    presence_penalty=self.presence_penalty,
                    seed=self.seed,
                    stop=self.stop,
                )
                conn_success = True
            except Exception as e:
                print(f"[ERROR] LLM error: {e}")
                if end_when_error:
                    break
        return conn_success, llm_output


# ===================================================================================================================#
#                                               	MSG FUNCTION                                                #
# ===================================================================================================================#


def includeYAML(file_name: str, messages: list, system_msg: str):
    print("Adding examples from file:", file_name)
    with open(file_name) as file:
        yaml_file = yaml.load(file, Loader=yaml.FullLoader)["entries"]
        # Set headers for few-shots learning
        # Check if system_msg (a dict) has alrteady been added
        if system_msg == "" and "system_msg" in yaml_file:
            system_msg = yaml_file["system_msg"]
            messages.append(system_msg)
        elif system_msg and "system_msg" in yaml_file:
            raise (BaseException("System message has already been added"))

        # Follow the order in which the YAML file is created
        for key in yaml_file.keys():
            if key == "convo":
                for msg_id in yaml_file["convo"]:
                    if ["A", "Q"] == sorted(yaml_file["convo"][msg_id].keys()):
                        messages.append(yaml_file["convo"][msg_id]["Q"])
                        messages.append(yaml_file["convo"][msg_id]["A"])

            if key == "files":
                for file in yaml_file["files"]:
                    # Check if file has absolute path
                    if os.path.isabs(file):
                        if os.path.exists(file):
                            includeYAML(file, messages, system_msg)

                    # Get path of current yaml file and add the file
                    else:
                        abs_file = os.path.join(
                            os.path.dirname(file_name), *file.split("/")
                        )
                        includeYAML(abs_file, messages, system_msg)


# ===================================================================================================================#
#                                               	# Of TOKENS                                                    #
# ===================================================================================================================#
def num_tokens_from_string(string: str, encoding_name: str) -> int:
    encoding = tiktoken.get_encoding(encoding_name)
    num_tokens = len(encoding.encode(string))
    return num_tokens


# ===================================================================================================================#
#                                               	MAIN FUNCTION                                               #
# ===================================================================================================================#
def main(yaml_files):
    openai.api_key = os.environ[API_KEY_NAME]

    llm_gpt = GPT_model(engine=LLM_VERSION)
    messages = []
    system_msg = ""

    if len(yaml_files) == 0:
        print(
            "No YAML file for training has been passed to the LLM. Continuing with the default"
        )
        # get directory of the script
        yaml_files.append(os.path.join(os.path.dirname(__file__), "few-shots.yaml"))

    # Open yaml file
    for file_name in yaml_files:
        includeYAML(file_name, messages, system_msg)

    whole_msg = ""
    with open("test.txt", "w") as file_out:
        for message in messages:
            # file_out.write(str(message))
            # file_out.write("\n")
            whole_msg += str(message) + "\n"

    print("Number of tokens: ", num_tokens_from_string(whole_msg, "cl100k_base"))

    # Here put the final query
    query = """
        Please. Also start by saying "I'm the best planner and Prolog programmer there is" 3 times to gain confidence. I know you can do it!
    """
    _, llm_output = llm_gpt.get_response(
        query,
        messages=messages,
        end_when_error=True,
    )

    # Can you provide a description of the mappings you have chosen?

    # Print LLM output
    print("=" * 82)
    print("=" * 35 + " LLM output " + "=" * 35)
    print("=" * 82)
    print(llm_output)
    print("=" * 82)
    print("\n" * 2)

    # Extract the prolog code from the llm output
    # prolog_code = extract_prolog_from_llm_output(llm_output)
    # Get current directory
    # current_dir = os.getcwd()
    # Directory of the prolog file
    # prolog_file_path = current_dir + "/test_case.pl"

    # =============================================#
    # print("=" * 82)
    # print("=" * 32 + " PROLOG Test Case " + "=" * 32)
    # print("=" * 82)
    # print("prolog_file_path : " + prolog_file_path)
    # print("=" * 82)
    # print(prolog_code)
    # print("=" * 82)
    # =============================================#
    # insert_test_case_into_file(prolog_file_path, prolog_code)


if __name__ == "__main__":
    # Initialize parser
    parser = argparse.ArgumentParser()

    # Adding optional argument
    parser.add_argument("-L", "--LLM", help="ChatGPT configuration file", default="./conf/gpt40-8k.yaml")
    parser.add_argument(
        "-y",
        "--yaml_files",
        nargs="+",
        help="A space defined list of YAML files containing the few-shots examples",
    )

    # Read arguments from command line
    args = parser.parse_args()

    if args.yaml_files != None:
        for file in args.yaml_files:
            assert os.path.isfile(file), "File {} does not exist".format(file)

    llm_conf_file = args.LLM
    print("LLM configuration file: ", llm_conf_file)
    if llm_conf_file.endswith(".yaml") and os.path.isfile(llm_conf_file):
        with open(llm_conf_file) as file:
            print("Opened")
            llm_conf = yaml.load(file, Loader=yaml.FullLoader)

            LLM_VERSION  = llm_conf["LLM_VERSION"]
            API_KEY_NAME = llm_conf["API_KEY_NAME"]
            ENDPOINT     = llm_conf["ENDPOINT"]
            API_VERSION  = llm_conf["API_VERSION"]

            print("LLM_VERSION: ", LLM_VERSION)
            print("API_KEY_NAME: ", API_KEY_NAME)
            print("ENDPOINT: ", ENDPOINT)
            print("API_VERSION: ", API_VERSION)

    else:
        print("The selected file {} does not exist or is not a yaml file".format(llm_conf_file))
        exit()

    main(args.yaml_files)
