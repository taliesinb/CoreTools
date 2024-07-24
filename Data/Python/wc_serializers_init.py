# this file mirrors wolframclient/serializers/__init__.py
# it defines ct_export to mirror wolframclient.serializers.export

from wc_serializers_encoder import ct_wolfram_encoder
import wolframclient.serializers

if not hasattr(wolframclient.serializers, 'export_original'):
	wolframclient.serializers.export_original = wolframclient.serializers.export

# ct_export mirrors wolframclient.serializers.export
def ct_export(data, stream=None, target_format=wolframclient.serializers.DEFAULT_FORMAT, **options):
	if not target_format in wolframclient.serializers.available_formats:
		raise ValueError(
			"Invalid export format %s. Choices are: %s"
			% (target_format, ", ".join(wolframclient.serializers.available_formats.keys()))
		)
	options.setdefault('encoder', ct_wolfram_encoder)
	if options['encoder'] is ct_wolfram_encoder:
		try:
			ct_wolfram_encoder.seen_ids = set()
			thing = wolframclient.serializers.available_formats[target_format](**options)
			return thing.export(data, stream=stream)
		finally:
			ct_wolfram_encoder.seen_ids = None
	return wolframclient.serializers.available_formats[target_format](**options).export(data, stream=stream)

