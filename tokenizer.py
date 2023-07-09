import sys
import tiktoken

def tokenize(text, model):
    enc = tiktoken.encoding_for_model(model)
    tokens = enc.encode(text)
    # we need to convert this to associated strings
    print(tokens)

if __name__ == "__main__":
    text = sys.argv[1]
    model = sys.argv[2]
    tokenize(text, model)
