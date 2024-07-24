from wolframclient.serializers import wolfram_encoder
from wolframclient.language import wl
from wolframclient.serializers import export

if 'torch_tensor_encoder' in globals():
    wolfram_encoder.unregister(torch.Tensor)
else:
    import torch

@wolfram_encoder.dispatch(torch.Tensor)
def torch_tensor_encoder(serializer, t):
    return serializer.encode(wl.TorchTensor(t.detach().numpy()))
