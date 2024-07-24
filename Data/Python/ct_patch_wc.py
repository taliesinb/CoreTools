import wolframclient
import wolframclient.utils
import wolframclient.utils.externalevaluate
import wolframclient.serializers
import wolframclient.language.decorators

from wc_serializers_encoder import ct_wolfram_encoder
from wc_serializers_init import ct_export
from wc_language_decorators import ct_safe_wl_execute
from wc_utils_externalevaluate import ct_SocketWriter_send_side_effect

# it's dangerous to patch these guy, i don't do that anymore
# wolframclient.serializers.export = ct_export
# wolframclient.serializers.wolfram_encoder = ct_wolfram_encoder
# wolframclient.language.decorators.export = ct_export

# instead we only need to patch send_side_effect, which we use for printing, and the
# outermost safe_wl_excute that is used by the ZeroMQ loop
wolframclient.language.decorators.safe_wl_execute = ct_safe_wl_execute
wolframclient.utils.externalevaluate.SocketWriter.send_side_effect = ct_SocketWriter_send_side_effect

