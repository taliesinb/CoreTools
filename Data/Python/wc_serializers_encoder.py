# this file mirrors wolframclient/serializers/encoder.py
# it defines ct_wolfram_encoder to mirror wolframclient.serializers.wolfram_encoder

import wolframclient
import wolframclient.serializers

import inspect
if hasattr(inspect, 'getfullargspec'):
	inspect_args = inspect.getfullargspec
elif hasattr(inspect, 'getargspec'):
	inspect_args = inspect.getargspec

class CoreToolsDispatch(wolframclient.serializers.encoder.WolframDispatch):
	def __init__(self):
		super().__init__()
		self.seen_ids = None

	def resolve(self, arg):
		if self.seen_ids is not None:
			arg_id = id(arg)
			if arg_id in self.seen_ids:
				return serialize_backref
			if hasattr(arg, "__dataclass_fields__"):
				self.seen_ids.add(arg_id)
		# if hasattr(arg, "_asdict"):
		# 	return serialize_asdict
		return super().resolve(arg)

# ct_wolfram_encoder mirrors wolframclient.serializers.wolfram_encoder
ct_wolfram_encoder = CoreToolsDispatch()

ct_wolfram_encoder.register_modules(
    # builtin libraries
    sys="wolframclient.serializers.encoders.builtin.encoder",
    decimal="wolframclient.serializers.encoders.decimal.encoder",
    datetime="wolframclient.serializers.encoders.datetime.encoder",
    fractions="wolframclient.serializers.encoders.fractions.encoder",
    # wolfram language support
    numpy="wolframclient.serializers.encoders.numpy.encoder",
)

ct_wolfram_encoder.register_plugins()

@ct_wolfram_encoder.dispatch(object)
def serialize_object(self, obj):
	if type(obj) is not type:
		if hasattr(obj, "__dataclass_fields__"):
			return serialize_dataclass_instance(self, obj)
	if callable(obj):
		return serialize_callable(self, obj)
	return self.serialize_external_object(obj)
	return serialize_to_pyobject(self, obj)

def serialize_to_pyobject(self, obj, *args):
	head = self.serialize_symbol(b'PyObject')
	obj_id = id(obj)
	head = self.serialize_function(head, (self.serialize_string(type(obj).__name__), self.serialize_int(obj_id)))
	return self.serialize_function(head, args)

def serialize_dataclass_instance(self, obj):
	return serialize_to_pyobject(self, obj,
		self.serialize_association(
			(self.encode(str(key)), self.encode(getattr(obj, key)))
			for key, value in obj.__dataclass_fields__.items()
		)
	)

def serialize_callable(self, obj):
	head = self.serialize_symbol(b'PyFunction')
	name =  obj.__name__ if hasattr(obj, "__name__") else obj.__class__.__name__
	cmd = self.encode(wolframclient.utils.encoding.force_text(name))
	args = self.encode(tuple(map(wolframclient.utils.encoding.force_text, inspect_args(obj)[0])))
	return self.serialize_function(head, (cmd, args))


def serialize_backref(self, obj):
	head = self.serialize_symbol(b'PyObjectRef')
	obj_id = id(obj)
	return self.serialize_function(head, (self.serialize_string(type(obj).__name__), self.serialize_int(obj_id)))

def serialize_asdict(self, obj):
	return serialize_to_pyobject(self, obj,
		self.serialize_association(obj._asdict())
	)

