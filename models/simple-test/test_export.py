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


if __name__ == "__main__":
    # jit_test()
    # jit_roberta()

    tokenizer = RobertaTokenizerFast.from_pretrained("roberta-base")
    configuration = RobertaConfig()
    roberta = RobertaModel(configuration)
    roberta.eval()
    tokens = tokenizer("hi how are you doing. this is a test.")
    ids = torch.tensor(tokens['input_ids']).reshape(1, len(tokens['input_ids']))
    (last_hidden_state, pooler_output) = roberta (ids)
    traced = torch.jit.trace(roberta , ids)
    traced.save('roberta_traced.zip')
    print(roberta(ids)[0][0][0][0:10])

    # sanity check
    (last_hidden_state_t, pooler_output_t) = traced(ids)
    diffs_lhs, diffs_pool = (last_hidden_state_t - last_hidden_state, pooler_output_t - pooler_output)

    print(ids)
    print(roberta(ids))
    print(traced(ids))
    print(diffs_lhs)

    print(torch.median(abs(diffs_lhs)))
    print(torch.median(abs(diffs_pool)))

    m = roberta(torch.tensor([[10]]))[0]
    m2 = roberta(torch.tensor([[10]]))[0]
    s = traced(torch.tensor([[10]]))[0]

    print(torch.median(abs(m)))
    print(torch.median(abs(s)))
    print(torch.median(abs(m-s)))


