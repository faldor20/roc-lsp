# interface Wrap
#     exposes [
#         to,
#         from

#     ]
#     imports [
#         DecodeUtils,
#     ]


# to=\val-> toType val
# from=\val-> fromType val


# Wrapper := {hi:Str}
#     implements [
#         From {fromType:wrapperGetter },
#         To {toType: wrapperFromer},
#         Eq,
        
#     ]
# wrapperGetter=\@Wrapper wrapper->wrapper
# wrapperFromer=\wrapper->@Wrapper wrapper

# expect
#     intern={hi:"hi"}
#     test=@Wrapper intern 
#     intern==from test
# expect
#     intern={hi:"hi"}
#     test=@Wrapper intern 
#     test==to intern


# To implements
#     toType: other -> val where val implements To
# From implements
#     fromType : val->other where val implements From
