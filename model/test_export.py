import torch
from transformers import RobertaTokenizer, RobertaConfig, RobertaModel

class Model(torch.nn.Module):
    def __init__(self):
        super(Model, self).__init__()
        self.fc1 = torch.nn.Linear(2, 1)
        self.fc1.weight.data = torch.tensor([[2.0, 3.0]])
        self.fc1.bias.data.fill_(0.0)

    def forward(self, x):
        return self.fc1(x)

if __name__ == "__main__":
    # test = Model()
    # traced = torch.jit.trace(test, torch.ones([1, 2]))
    # traced.save('traced.zip')

    tokenizer = RobertaTokenizer.from_pretrained("roberta-base")
    configuration = RobertaConfig()
    model = RobertaModel(configuration)
    tokenizer("Hello world")['input_ids']
    tokens = tokenizer("Hello world. This is a test sentence")
    tokens = tokenizer(["Hello world. This is a test sentence", "and this is another sentence", "hi i am here"])
    ids = torch.tensor(tokens['input_ids'][0]).reshape(1, 10)

    None


