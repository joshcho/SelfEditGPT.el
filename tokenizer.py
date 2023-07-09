import sys
import tiktoken

def tokenize(text, model):
    enc = tiktoken.encoding_for_model(model)
    tokens_bytes = enc.decode_tokens_bytes(enc.encode(text))

    # Convert bytes to string
    tokens = [token_byte.decode('utf-8') for token_byte in tokens_bytes]

    # Convert Python list to Emacs Lisp list format
    emacs_list_format = '(' + ' '.join(f'"{token}"' for token in tokens) + ')'
    print(emacs_list_format)

if __name__ == "__main__":
    text = sys.argv[1]
    model = sys.argv[2]
    tokenize(text, model)
