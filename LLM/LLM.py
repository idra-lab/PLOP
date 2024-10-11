import yaml
import os

from openai import AzureOpenAI
from retry import retry

try:
    import utility
except ImportError:
    from . import utility


class LLM:
    llm_default_config = {
        "max_tokens": 4096,
        "temperature": 0.0,
        "top_p": 0.0,
        "frequency_penalty": 0.0,
        "presence_penalty": 0.0,
        "stop": None,
        "seed": 42
    }

    def __init__(self, llm_connection_config_file = os.path.join(os.path.dirname(__file__), "conf/gpt40-32k.yaml"), examples_yaml_file = [""], llm_config = None):
        self.messages = []
        self.system_msg = ""

        if len(examples_yaml_file) > 0 :
            for file in examples_yaml_file:
                if file == "":
                    continue
                assert os.path.isfile(file), "File {} does not exist".format(file)
                tmp_messages = []
                utility.includeYAML(file, tmp_messages, self.system_msg)
                self.messages += tmp_messages
        else:
            print("No examples provided")

        print("LLM configuration file: ", llm_connection_config_file)
        if llm_connection_config_file.endswith(".yaml") and os.path.isfile(llm_connection_config_file):
            with open(llm_connection_config_file) as file:
                llm_connection_config = yaml.load(file, Loader=yaml.FullLoader)

                self.engine  = llm_connection_config["LLM_VERSION"]
                self.API_KEY_NAME = llm_connection_config["API_KEY_NAME"]
                self.ENDPOINT     = llm_connection_config["ENDPOINT"]
                self.API_VERSION  = llm_connection_config["API_VERSION"]

                print("LLM_VERSION: ", self.engine)
                print("API_KEY_NAME: ", self.API_KEY_NAME)
                print("ENDPOINT: ", self.ENDPOINT)
                print("API_VERSION: ", self.API_VERSION)

        else:
            raise FileNotFoundError("The selected file {} does not exist or is not a yaml file".format(llm_connection_config_file))
        
        self.__config(llm_config)

    def __config(self, config = None):
        if config is None:
            config = LLM.llm_default_config
        else:
            for key in LLM.llm_default_config:
                if key not in config:
                    raise ValueError("Missing key in config: {}".format(key))

        self.max_tokens        = config['max_tokens']
        self.temperature       = config['temperature']
        self.top_p             = config['top_p']
        self.frequency_penalty = config['frequency_penalty']
        self.presence_penalty  = config['presence_penalty']
        self.stop              = config['stop']
        self.seed              = config['seed'] 


    def query(self, prompt, end_when_error=False, max_retry=5) -> tuple:
        conn_success, llm_output = False, ""
        
        self.messages.append({"role": "user", "content": prompt})

        if not os.path.exists("output"):
            os.makedirs("output")
        with open(os.path.join("output", "sent_query.txt"), "w") as f:
            for msg in self.messages:
                f.write(f"{msg['role']}: {msg['content']}\n")
        
        n_retry = 0
        while not conn_success:
            n_retry += 1
            if n_retry >= max_retry:
                break
            try:
                print("[INFO] Connecting to the LLM ...")
                llm_output = self.__connect_openai(self.messages)
                conn_success = True
            except Exception as e:
                print(f"[ERROR] LLM error: {e}")
                if end_when_error:
                    break

        self.messages = self.messages[:-1]
        
        return conn_success, llm_output
    
    
    @retry(tries=2, delay=30)
    def __connect_openai(self, messages):
        # UNITN-key
        client = AzureOpenAI(
            api_key        = os.environ[self.API_KEY_NAME],
            azure_endpoint = self.ENDPOINT,
            api_version    = self.API_VERSION,
        )

        response = client.chat.completions.create(
            model             = self.engine,
            messages          = messages,
            temperature       = self.temperature,
            max_tokens        = self.max_tokens,
            top_p             = self.top_p,
            frequency_penalty = self.frequency_penalty,
            presence_penalty  = self.presence_penalty,
            seed              = self.seed,
            stop              = self.stop,
        )

        return response.choices[0].message.content
    

