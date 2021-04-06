import torch
from transformers import RobertaTokenizerFast, RobertaConfig, RobertaModel
import umap
import altair

class Model(torch.nn.Module):
    def __init__(self):
        super(Model, self).__init__()
        self.fc1 = torch.nn.Linear(2, 1)
        self.fc1.weight.data = torch.tensor([[2.0, 3.0]])
        self.fc1.bias.data.fill_(0.0)

    def forward(self, x):
        return self.fc1(x)

def jit_test():
    test = Model()
    traced = torch.jit.trace(test, torch.ones([1, 2]))
    traced.save('traced.zip')

def jit_roberta():
    # tokenizer = RobertaTokenizer.from_pretrained("roberta-base")
    tokenizer = RobertaTokenizerFast.from_pretrained("roberta-base")
    configuration = RobertaConfig()
    model = RobertaModel(configuration)
    tokens = tokenizer("hi how are you doing. this is a test.")
    ids = torch.tensor(tokens['input_ids']).reshape(1, len(tokens['input_ids']))
    (last_hidden_state, pooler_output) = model(ids)
    traced = torch.jit.trace(model, ids)
    (last_hidden_state_t, pooler_output_t) = traced(ids)
    diffs_lhs, diffs_pool = (last_hidden_state_t - last_hidden_state, pooler_output_t - pooler_output)
    # these diffs don't look good.
    traced.save('roberta_traced.zip')


if __name__ == "__main__":
    # jit_test()
    # jit_roberta()

    tokenizer = RobertaTokenizerFast.from_pretrained("roberta-base")
    configuration = RobertaConfig()
    model = RobertaModel(configuration)
    tokens = tokenizer("hi how are you doing. this is a test.")
    ids = torch.tensor(tokens['input_ids']).reshape(1, len(tokens['input_ids']))
    (last_hidden_state, pooler_output) = model(ids)
    traced = torch.jit.trace(model, ids)
    (last_hidden_state_t, pooler_output_t) = traced(ids)
    diffs_lhs, diffs_pool = (last_hidden_state_t - last_hidden_state, pooler_output_t - pooler_output)

    print(ids)
    print(traced(ids))


