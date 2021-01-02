import torch
import torch.nn as nn

class TestModule(nn.Module):
    def __init__(self):
        super(TestModule, self).__init__()

        self.fc1 = nn.Linear(2, 1)
        self.setval()
        # self.fc1.weight.data = torch.tensor([[2.0, 3.0]])

    def forward(self, x):
        y = self.fc1(x)
        # y = torch.relu(y)
        return y

    def setval(self):
        self.fc1.weight.data = torch.tensor([[2.0, 3.0]])
        self.fc1.bias.data.fill_(0.0)


if __name__ == "__main__":
    test = TestModule()
    x = torch.tensor([1.0, 2.0])
    test.setval()
    print(test(x))

    traced = torch.jit.trace(TestModule(), x)
    print(traced.code)
    print(traced.graph)

    scripted = torch.jit.script(test)
    print(scripted.code)
    print(scripted.graph)

    traced.save('traced.zip')




