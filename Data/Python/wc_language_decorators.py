# this file mirrors wolframclient/language/decorators.py
# it defines ct_safe_wl_execute to mirror wolframclient.language.decorators.safe_wl_execute

import sys
import wolframclient

from wc_serializers_init import ct_export

# ct_safe_wl_execute mirrors wolframclient.language.decorators.safe_wl_execute
def ct_safe_wl_execute(function, args=(), opts={}, export_opts={}, exception_class=None):
    try:
        return ct_export(function(*args, **opts), **export_opts)

    except Exception as export_exception:
        # print('function = ', str(function))
        # print('args = ', str(args))
        # print('opts = ', str(opts))
        # print('eopts = ', str(export_opts))
        # print('exception: ', str(export_exception), type(export_exception).__name__)
        try:
            try:
                # The user can provide an exception class, and it can be broken, in which case we are running another
                # try / except to return errors that are happening during class serialization

                if isinstance(export_exception, wolframclient.language.exceptions.WolframLanguageException):
                    try:
                        export_exception.set_traceback(*sys.exc_info())
                        return wolframclient.serializers.export(export_exception, **export_opts)
                    except Exception:
                        pass

                if not exception_class or exception_class is wolframclient.language.exceptions.WolframLanguageException:
                    return wolframclient.serializers.export(
                        wolframclient.language.exceptions.WolframLanguageException(export_exception, exec_info=sys.exc_info()),
                        **export_opts
                    )

                # A custom error class might fail, if this is happening then we can try to use the built in one
                return wolframclient.serializers.export(
                    exception_class(export_exception, exec_info=sys.exc_info()), **export_opts
                )
            except Exception as exception_export_err:
                return wolframclient.serializers.export(
                    wolframclient.language.exceptions.WolframLanguageException(exception_export_err, exec_info=sys.exc_info()),
                    target_format=export_opts.get("target_format", wolframclient.serializers.DEFAULT_FORMAT),
                    encoder="wolframclient.serializers.encoders.builtin.encoder",
                )

        except Exception as unknown_exception:
            # print('dbl exception: ', str(unknown_exception), type(unknown_exception).__name__)
            # This is the last resort.
            # Everything went wrong, including the code that was supposed to return a traceback, or the custom
            # normalizer is doing something it should not. This should never happen.
            try:
                return wolframclient.serializers.export(
                    wolframclient.language.wl.Failure(
                        "PythonFailure",
                        {
                            "MessageTemplate": wolframclient.utils.encoding.safe_force_text(unknown_exception),
                            "MessageParameters": {},
                            "FailureCode": wolframclient.utils.encoding.safe_force_text(
                                unknown_exception.__class__.__name__
                            ),
                            "Traceback": wolframclient.utils.encoding.force_text(traceback.format_exc()),
                        },
                    ),
                    target_format=export_opts.get("target_format", wolframclient.serializers.DEFAULT_FORMAT),
                    encoder="wolframclient.serializers.encoders.builtin.encoder",
                )
            except Exception:
                # Something went worst.
                # this might happen with import errors / syntax errors in third party pluging that are loading the
                # exporter and doing some real damage to the dispatcher we are using.
                return wolframclient.language.decorators.DEFAULT_UNKNOWN_FAILURE[
                    export_opts.get("target_format", wolframclient.serializers.DEFAULT_FORMAT)
                ]
