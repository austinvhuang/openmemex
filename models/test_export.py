import torch

class Model(torch.nn.Module):
    def __init__(self):
        super(Model, self).__init__()
        self.fc1 = torch.nn.Linear(2, 1)
        self.fc1.weight.data = torch.tensor([[2.0, 3.0]])
        self.fc1.bias.data.fill_(0.0)

    def forward(self, x):
        return self.fc1(x)

if __name__ == "__main__":
    test = Model()
    traced = torch.jit.trace(test, torch.ones([1, 2]))
    traced.save('traced.zip')