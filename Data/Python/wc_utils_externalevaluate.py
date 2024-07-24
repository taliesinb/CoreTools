# this file mirrors wolframclient/utils/externalevaluate.py

from wc_serializers_init import ct_export

def ct_SocketWriter_send_side_effect(self, expr):
	self.write(ct_export(self.keep_listening(expr), target_format="wxf", allow_external_objects=True))